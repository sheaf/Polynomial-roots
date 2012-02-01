{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Configuration.Parsing where

import Overture hiding (between, (<|>), many)
import Prelude ()
import Data.Char
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Configuration
import Rendering.Coord hiding (elem)
import Debug.Trace

type ParseErr = String

parseConfig :: (Monad m) => String -> String -> m (Either ParseErr (Configuration n c))
parseConfig name str = liftM (left show) (runParserT pConfiguration () name str)
                       


pConfiguration :: (Monad m) => ParsecT String u m (Configuration n c)
pConfiguration = do md <- pField "run-mode" *> pRunMode
                    many1 newline
                    rds <- many1 pRender
                    eof
                    return $ Cfg md rds

pRunMode :: (Monad m) => ParsecT String u m RunMode
pRunMode = pString "gui" *> return WithGUI
       <|> pString "file" *> return ImageFile

pRender :: (Monad m) => ParsecT String u m (Render n c)
pRender = do md <- pField "render" *> pRenderMode <* pString "{" 
             many newline
             ctr <- pField "center" *> pCd2 pDouble
             pFieldSep
             rSz <- pField "size" *> pCd2 pDouble
             pFieldSep
             let rSpec = RenderSpec md ctr rSz
             sz <- pField "output-size" *> pCd2 pInt
             pFieldSep
             fixAsp <- pField "fix-aspect" *> pEnumerated
             pFieldSep
             gSpec <- pField "gradient" *> pGradSpec 
             optional $ pFieldSep
             pString "}"
             spaces
             return $ Render rSpec Nothing sz fixAsp gSpec

pFieldSep :: (Monad m) => ParsecT String u m String
pFieldSep = many1 pNewline
        <|> pGenToken (char ';') *> many pNewline

pNewline :: (Monad m) => ParsecT String u m Char
pNewline = pGenToken newline 

pGradSpec :: (Monad m) => ParsecT String u m (GradientSpec n c)
pGradSpec = pGradName 
        <|> pParens pGradSpecExpr

pGradSpecExpr :: (Monad m) => ParsecT String u m (GradientSpec n c)
pGradSpecExpr = choice [pGradName] --, pGradSplit, pGradCmb, pGradTrans]

pGradName :: (Monad m) => ParsecT String u m (GradientSpec n c)
pGradName = NamedGradient <$> pName

{-
pGradSplit :: (Monad m) => ParsecT String u m GradientSpec
pGradSplit = do pString "split"
                pString "["
                exp <- pGradSpec
                k <- pGradSplitList exp
                return $ k id

pGradSplitList :: (Monad m) => GradientSpec -> ParsecT String u m (
    ([(GradientSpec, Double)] -> [(GradientSpec, Double)]) -> GradientSpec)
pGradSplitList exp = pGradSplitStep exp
                 <|> pString "]" *> return (\f -> Split (f []) exp)

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

pGradTrans :: (Monad m) => ParsecT String u m GradientSpec
pGradTrans = Transform <$> pGradTransFunc <*> pGradSpec

pGradTransFunc :: (Monad m) => ParsecT String u m TransFunc
pGradTransFunc = choice [ pString "inv" *> return Invert
                        , pString "exp" *> (Exponent <$> pDouble)
                        , pString "rev" *> return Reverse
                        ]
-}

pRenderMode :: (Monad m) => ParsecT String u m RenderMode
pRenderMode = Roots <$> (pString "roots" *> pInt)
          <|> IFS   <$> (pString "ifs"   *> pInt)

pCd2 :: (Monad m) => ParsecT String u m a -> ParsecT String u m (Cd2 a)
pCd2 p = mkCd2 <$> p <*> p

pEnumerated :: (Monad m, Enum a, Bounded a, Show a) => ParsecT String u m a
pEnumerated = pByShow enumerate

pField :: (Monad m) => String -> ParsecT String u m String
pField str = pString str <* pString ":"

pString :: (Monad m) => String -> ParsecT String u m String
pString str = pGenToken $ insensitiveString str

pInt :: (Monad m) => ParsecT String u m Int
pInt = pGenToken intVal

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

intVal :: (Monad m) => ParsecT String u m Int
intVal = fromJust . read <$> many digit

doubleVal :: (Monad m) => ParsecT String u m Double
doubleVal = do ds1 <- many1 digit
               ds2 <- option [] (char '.' *> (('.':) <$> many1 digit))
               return (fromJust . read $ ds1 ++ ds2)


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
eatComments = optional $ (string "--" *> manyTill anyChar (lookAhead newline) <?> "comment")

inlineSpaces :: (Monad m) => ParsecT String u m String
inlineSpaces = many spaceChar

isKeyword :: String -> Bool
isKeyword str = str `elem` keywords

keywords :: [String]
keywords = [ "run-mode", "render", "center", "size", "fix-aspect"
           , "output-size", "output-file", "gradient", "split"
           , "inv", "exp", "rev", "blend", "overlay"
           ]

between' outer = between outer outer
