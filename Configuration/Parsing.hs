{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Configuration.Parsing where

import Overture hiding (between, (<|>), many)
import Prelude ()
import Data.Char
import Data.Colour
import Data.Colour.SRGB
import Data.Maybe
import Data.Ratio
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Configuration
import Rendering.Coord hiding (elem)
import Debug.Trace

import Types

type ParseErr = String

--------------------------------------------------------------------------------

class (Coefficient a) => PCoefficient a where
    pCoeff :: (Monad m) => ParsecT String u m a

instance PCoefficient Int where
    pCoeff = pInt
instance PCoefficient Integer where
    pCoeff = pInt
instance PCoefficient Double where
    pCoeff = pDouble
instance PCoefficient (Complex Double) where
    pCoeff = (uncurry (:+)) <$> (pPair pDouble)
instance (PCoefficient a, Integral a) => PCoefficient (Ratio a) where
    pCoeff = pRatio pCoeff

--------------------------------------------------------------------------------

runParse :: (Monad m) 
          => ParsecT String () m a -> String -> String -> m (Either ParseErr a)
runParse parser name str = liftM (left show) (runParserT parser () name str)

pRunSpec :: (Monad m) => ParsecT String u m RunSpec
pRunSpec = do md <- pField "run-mode" *> pRunMode
              many1 newline
              pString "renders"
              many newline
              rds <- many1 pRenderSpec
              pString "mode"
              return $ RunSpec md rds

pRunMode :: (Monad m) => ParsecT String u m RunMode
pRunMode = pString "gui" *> return WithGUI
       <|> pString "file" *> return ImageFile

pRenderSpec :: (Monad m) => ParsecT String u m RenderSpec
pRenderSpec = do pString "{" 
                 many newline
                 ctr <- pField "center" *> pCd2 pDouble
                 pFieldSep
                 rSz <- pField "size" *> pCd2 pDouble
                 pFieldSep
                 let rSpec = WindowSpec ctr rSz
                 sz <- pField "output-size" *> pCd2 pInt
                 pFieldSep
                 fixAsp <- pField "fix-aspect" *> pEnumerated
                 optional $ pFieldSep
                 pString "}"
                 spaces
                 return $ RenderSpec rSpec Nothing sz fixAsp

pModeConfig :: (Monad m, PCoefficient a) => ParsecT String u m c -> ParsecT String u m (Config c a)
pModeConfig p = do manyTill anyChar (try (newline *> string "mode"))
                   manyTill anyChar (try $ pString "{")
                   many newline
                   cfs <- pField "coefficients" *> pList pCoeff
                   pFieldSep
                   res <- pField "resolution" *> pPair pNat
                   pFieldSep
                   deg <- pField "degree" *> pNat
                   pFieldSep
                   ctr <- pField "center" *> ( uncurry (:+) <$> pPair pDouble)
                   pFieldSep
                   w <- pField "width" *> pDouble
                   pFieldSep
                   s <- pField "scaling" *> pScaling
                   pFieldSep
                   many newline
                   pString "colouring"
                   many newline
                   col <- p
                   optional $ pFieldSep
                   many newline
                   pString "}"
                   return (Config cfs res deg ctr w s col)

pDensityCol :: (Monad m) => ParsecT String u m DensityCol
pDensityCol = do many newline
                 pString "{"
                 many newline
                 grad <- pField "gradient" *> pGradSpec
                 pFieldSep
                 op <- pField "density" *> pDouble
                 optional $ pFieldSep
                 many newline
                 pString "}"
                 return $ (grad, op)

pSourceCol :: (Monad m) => ParsecT String u m a -> ParsecT String u m (SourceCol a)
pSourceCol pCf = do many newline
                    pString "{"
                    many newline
                    method <- (pField "method" *> pStrings ["1","2"])
                    pFieldSep
                    coeffs <- pField "coefficients" *> pList pCf
                    pFieldSep
                    op <- pField "opacity" *> pDouble
                    optional $ pFieldSep
                    many newline
                    pString "}"
                    return $ (method, coeffs, op)

pFieldSep :: (Monad m) => ParsecT String u m String
pFieldSep = many1 pNewline
        <|> pGenToken (char ';') *> many pNewline

pNewline :: (Monad m) => ParsecT String u m Char
pNewline = pGenToken newline 

pBool :: (Monad m) => ParsecT String u m Bool
pBool = do try $ pStrings ["yes", "y", "true", "t"]
           return True
       <|> do pStrings ["no", "n", "false", "f"]
              return False

pEither :: (Monad m) => ParsecT String u m a -> ParsecT String u m b -> ParsecT String u m (Either a b)
pEither p q = Left <$> try p
          <|> Right <$> q

pScaling :: (Monad m) => ParsecT String u m (Either Bool Double)
pScaling = pEither pBool pDouble

pGradSpec :: (Monad m) => ParsecT String u m GradientSpec
pGradSpec = pGradName
        <|> pParens pGradSpecExpr

pGradSpecExpr :: (Monad m) => ParsecT String u m GradientSpec
pGradSpecExpr = choice [pGradName, pGradSplit, pGradCmb, pGradCollate]

pGradName :: (Monad m) => ParsecT String u m GradientSpec
pGradName = NamedGradient <$> pName

pGradSplit :: (Monad m) => ParsecT String u m GradientSpec
pGradSplit = do pString "split"
                pString "["
                exp <- pGradSpec
                k <- pGradSplitList exp
                return $ k id

pGradSplitList :: (Monad m) => GradientSpec -> ParsecT String u m (
    ([(GradientSpec, Double)] -> [(GradientSpec, Double)]) -> GradientSpec)
pGradSplitList exp = pGradSplitStep exp
                 <|> pString "]" *> return (\f -> Split (f []))

pGradSplitStep :: (Monad m) => GradientSpec -> ParsecT String u m (
    ([(GradientSpec, Double)] -> [(GradientSpec, Double)]) -> GradientSpec)
pGradSplitStep exp = do dbl <- pDouble
                        gExpr <- pGradSpec
                        k <- pGradSplitList gExpr
                        return $ \f -> k (f . ((exp, dbl):))

pGradCmb :: (Monad m) => ParsecT String u m GradientSpec
pGradCmb = do blFunc <- pEnumerated
              gExps <- pBrackets $ pGradSpecExpr `sepBy1` pString ","
              return $ Combine blFunc gExps

pGradCollate :: (Monad m) => ParsecT String u m GradientSpec
pGradCollate = Collate <$> pList (pTuple pColour pDouble) 

pColour :: (Monad m) => ParsecT String u m (AlphaColour Double)
pColour = do pString "#"
             hex <- count 6 (oneOf "0123456789ABCDEF")
             return $ colourFromHex hex
             --todo: add option for reading colour names instead
                 where colourFromHex :: String -> AlphaColour Double
                       colourFromHex hex = opaque $ sRGB r g b
                           where r' = take 2 hex
                                 g' = take 2 (drop 2 hex)
                                 b' = take 2 (drop 4 hex)
                                 [r,g,b] = map go [r',g',b']
                                    where go x = fromJust . read $ "0x" ++ x

pCd2 :: (Monad m) => ParsecT String u m a -> ParsecT String u m (Cd2 a)
pCd2 p = mkCd2 <$> p <*> p

pTuple :: (Monad m) => ParsecT String u m a -> ParsecT String u m b -> ParsecT String u m (a,b)
pTuple p q = do pString "("
                spaces
                a <- p
                spaces
                pString ","
                spaces
                b <- q
                spaces
                pString ")"
                return (a,b)

pPair p = pTuple p p

pRatio :: (Monad m, Integral a) => ParsecT String u m a -> ParsecT String u m (Ratio a)
pRatio p = do n <- p
              pString "/"
              d <- p
              return $ n % d

pList :: (Monad m) => ParsecT String u m a -> ParsecT String u m [a]
pList p = do pString "["
             list <- p `sepBy` (pString ",")
             pString "]"
             return list

pEnumerated :: (Monad m, Enum a, Bounded a, Show a) => ParsecT String u m a
pEnumerated = pByShow enumerate

pField :: (Monad m) => String -> ParsecT String u m String
pField str = pString str <* pString ":"

pString :: (Monad m) => String -> ParsecT String u m String
pString str = pGenToken $ insensitiveString str

pStrings :: (Monad m) => [String] -> ParsecT String u m String
pStrings strs = choice (map (try . pString) strs)

pInt :: (Monad m, Integral a, Read a) => ParsecT String u m a
pInt = pGenToken intVal

pNat :: (Monad m) => ParsecT String u m Int
pNat = pGenToken natVal

pDouble :: (Monad m) => ParsecT String u m Double
pDouble = pGenToken doubleVal

pName :: (Monad m) => ParsecT String u m String
pName = pGenToken nameToken

pParens, pBrackets :: (Monad m) => ParsecT String u m a -> ParsecT String u m a
pParens p = pString "(" *> p <* pString ")"
pBrackets p = pString "[" *> p <* pString "]"

pByShow :: (Monad m, Show a) => [a] -> ParsecT String u m a
pByShow xs = choice $ map byShow xs
  where byShow x = try (pString (show x) *> return x)

pGenToken :: (Monad m) => ParsecT String u m a -> ParsecT String u m a
pGenToken p = between' inlineSpaces p <* eatComments

nameToken, quotedName, unQName, rawNameToken :: (Monad m) => ParsecT String u m String
nameToken = quotedName <|> try unQName
quotedName = between' (char '"') unQName
unQName = do n <- rawNameToken 
             if isKeyword n then unexpected ("keyword " ++ n) else return n
rawNameToken = ((:) <$> letter <*> many nameChar)

intVal :: (Monad m, Integral a, Read a) => ParsecT String u m a
intVal = do pm <- option ' ' (oneOf "+-")
            ns <- many1 digit
            return (fromJust . read $ [pm] ++ ns)

natVal :: (Monad m) => ParsecT String u m Int
natVal = fromJust . read <$> many1 digit

doubleVal :: (Monad m) => ParsecT String u m Double
doubleVal = do pm  <- option ' ' (oneOf "+-")
               ds1 <- many1 digit
               ds2 <- option [] (char '.' *> (('.':) <$> many1 digit))
               return (fromJust . read $ [pm] ++ ds1 ++ ds2)

insensitiveString :: (Monad m) => String -> ParsecT String u m String
insensitiveString str = try (mapM insensitiveChar str)
                    <?> str

insensitiveChar :: (Monad m) => Char -> ParsecT String u m Char
insensitiveChar c = satisfy $ (toLower c ==) . toLower

nameChar, nameSep, spaceChar :: (Monad m) => ParsecT String u m Char
nameChar = alphaNum <|> nameSep
nameSep = oneOf "-_" *> return ' '
spaceChar = oneOf " \t"

eatComments :: (Monad m) => ParsecT String u m ()
eatComments = optional $ (try (string "--") *> manyTill anyChar (lookAhead newline) <?> "comment")

inlineSpaces :: (Monad m) => ParsecT String u m String
inlineSpaces = many spaceChar

isKeyword :: String -> Bool
isKeyword str = str `elem` keywords

keywords :: [String]
keywords = [ "run-mode", "render", "center", "size", "fix-aspect"
           , "output-size", "output-file", "gradient", "split"
           , "inv", "exp", "rev", "blend", "overlay", "density"
           , "method", "coefficients", "degree", "opacity", "width"
           ]

between' outer = between outer outer
