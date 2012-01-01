{- Polynomial roots.
Computes the set of polynomials with a particular restricted set of coeffs,
who might have roots in a particular rectangle in the complex plane,
using tree pruning.

To do:
1) Fix parsing (ordering of questions, etc).
2) Allow non-monochrome images.
3) Figure out proper scaling constants.
4) Optimisation.
5) Investigate better bounds. -}

{-# LANGUAGE TypeFamilies, TypeSynonymInstances,
             FlexibleContexts, FlexibleInstances #-}

--------------------------------------------------------------------------------
--Imports.

import Control.Applicative
import Control.Exception(SomeException,IOException,handle)
import Data.Char(toLower)
import Data.Complex
import Data.List
import Data.Maybe
import Foreign.C.Types(CInt)
import qualified Graphics.GD.ByteString.Lazy as GD
import System.Environment(getArgs)
import System.IO

--------------------------------------------------------------------------------
--Basic datatypes.

type Degree = Int

--Intervals. More involved interval arithmetic later.
type RealInterval = (Double,Double)
type ComplexInterval = (Complex Double,Complex Double)

--Polynomials. Constant coefficient is the 0th term.
--Have to be able to coerce coefficients into the complex numbers,
--for polynomial evaluation.
class (Num a, Read a) => Coefficient a where
    toComplex :: a -> Complex Double
    toAbs :: a -> Double

instance Coefficient Int where 
    toComplex = fromIntegral
    toAbs = fromIntegral
instance Coefficient Double where
    toComplex x = x :+ 0
    toAbs = abs
instance Coefficient (Complex Double) where 
    toComplex = id
    toAbs = magnitude

--Polynomials as lists of coefficients.
type Polynomial a = [a]
type IterCoeffs a = [a]

--Trees of coefficients as representing polynomials.
type CanHaveRoot = Bool
data CoefficientTree a = Empty | Node a CanHaveRoot [CoefficientTree a]
    deriving(Eq,Show)

--Root finding types.
type Point = Complex Double
type Guess = Complex Double
type Root = Complex Double
type Iterations = Int
type ErrorBound = Double

--Iterated function systems.
type IFS = (Point -> [Point], [Point]) --second coordinate is starting values
    
--Plotting datatypes, and options.
type Resolution = (Int,Int)
type Center = Complex Double
type Width = Double
type Colour = [Int] --list of three ints; alpha channel not implemented yet.
type Gradient = (Int -> Colour,String) --the string is the label
type Pixel = (Int,Int)
data Mode = Roots | IFS | Both deriving(Eq)
data Config a = Config {
coefficients::IterCoeffs a, resolution::Resolution,
degree::Degree, center::Center, width::Width, gradient::Gradient}

--------------------------------------------------------------------------------
--Basic functions.

evaluate :: (Coefficient a, b ~ a) => Polynomial a -> b -> b
evaluate (a:as) z = a + z * (evaluate as z)
evaluate [] z = 0

derivative :: Coefficient a => Polynomial a -> Polynomial a
derivative = (zipWith (*) (map fromIntegral [1..])) . (drop 1)

--A bit dodgy, but useful.
filterClose :: Coefficient a => ErrorBound -> [a] -> [a]
filterClose eps [] = []
filterClose eps (c:cs) = (filterClose' eps cs [c])
    where filterClose' eps' [] bs = bs
          filterClose' eps' (a:as) bs
              | good = filterClose' eps' as (bs++[a])
              | otherwise = filterClose' eps' as bs
                   where good = all (> eps') (map (\b -> toAbs (b-a)) bs)

--------------------------------------------------------------------------------
--Interval arithmetic.

class (Fractional a, Fractional (Scalar a)) => Interval a where
    type Scalar a :: *
    intersects :: a -> a -> Bool
    elemI :: Scalar a -> a -> Bool
    (+!) :: Scalar a -> a -> a
    (!+) :: a -> Scalar a -> a
    (!+) = flip (+!)
    (-!) :: Scalar a -> a -> a
    (!-) :: a -> Scalar a -> a
    (!-) = (!+).negate
    (*!) :: Scalar a -> a -> a
    (!*) :: a -> Scalar a -> a
    (!*) = flip (*!)
    (/!) :: Scalar a -> a -> a
    (!/) :: a -> Scalar a -> a
    x !/ y = x !* recip(y)
    fromScalar :: Scalar a -> a

instance Num RealInterval where
    (x1,y1) + (x2,y2) = (x1+x2,y1+y2)
    (x1,y1) - (x2,y2) = (x1-y2,y1-x2)
    (x1,y1) * (x2,y2) = (mini, maxi)
        where mini = minimum [x1*x2,x1*y2,y1*x2,y1*y2]
              maxi = maximum [x1*x2,x1*y2,y1*x2,y1*y2]
    negate (x1,y1) = (-y1,-x1)
    abs (x1,y1) = (mini',maxi')
        where mini' = abs $ minimum [maximum [x1,0],y1]
              maxi' = maximum [abs x1,abs y1]
    signum (x1,y1)= error "No signum definition for RealInterval"
    fromInteger n = (fromInteger n::Double,fromInteger n::Double)

instance Fractional RealInterval where
    (x1,y1) / (x2,y2) = (x1,y1) * (recip(y1),recip(x1))
    fromRational q = (fromRational q ::Double, fromRational q::Double)

instance Interval RealInterval where
    type Scalar RealInterval = Double
    intersects (x1,y1) (x2,y2) = (x1 <= x2 && y1 >= x1)||(x1 >= x2 && x1 <= y2)
    a `elemI` (x1,y1) = (a >= x1 && a <= y1)
    a +! (x1,y1) = (x1+a,y1+a)
    a -! (x1,y1) = (a-y1,a-x1)
    a *! (x1,y1)
        | a > 0 = (a*x1,a*y1)
        | a < 0 = (a*y1,a*x1)
        | a == 0 = (0,0)
    a /! (x1,y1) = a *! (recip(y1), recip(x1))
    fromScalar a = (a,a)

minabsI :: ComplexInterval -> Double
minabsI (z,w)
    | (x <=0 && y <= 0 && x' >=0 && y' >=0) = 0
    | (x <=0 && y <= 0 && x' < 0) = minabsI(-w,-z)
    | (x <=0 && y <= 0 && y' < 0) = minabsI((-x):+y',(-x'):+y)
    | (x <= 0 && y > 0) = minabsI(-w,-z)
    | (x > 0 && y > 0) = sqrt $ x*x+y*y
    | otherwise = x
        where x = realPart z
              y = imagPart z
              x'= realPart w
              y'= imagPart w

maxabsI :: ComplexInterval -> Double
maxabsI (z,w) = sqrt $ maximum [x*x+y*y,x*x+y'*y',x'*x'+y*y,x'*x'+y'*y']
    where x = realPart z
          y = imagPart z
          x'= realPart w
          y'= imagPart w

absI :: ComplexInterval -> RealInterval
absI (z,w) = (minabsI (z,w),maxabsI (z,w) )

instance Num ComplexInterval where
    (z1,w1) + (z2,w2) = (z1+z2,w1+w2)
    (z1,w1) - (z2,w2) = (z1-w2,w1-z2)
    (z1,w1) * (z2,w2) = let
              mini = (minimum b1 - maximum b2) :+ (minimum b3 + minimum b4)
              maxi = (maximum b1 - minimum b2) :+ (maximum b3 + maximum b4)
              [b1,b2,b3,b4] = [[x1*x2,x1'*x2,x1*x2',x1'*x2'],
                              [y1*y2,y1'*y2,y1*y2',y1'*y2'],
                              [x1*y2,x1'*y2,x1*y2',x1'*y2'],
                              [y1*x2,y1'*x2,y1*x2',y1'*x2']]
              [x1,x1',x2,x2'] = (map realPart) [z1,w1,z2,w2]
              [y1,y1',y2,y2'] = (map imagPart) [z1,w1,z2,w2]
              in (mini,maxi)
    negate (z1,w1) = (-w1,-z1)
    abs (z,w) = (minabsI(z,w) :+ 0,maxabsI(z,w) :+ 0)
    signum = error "No signum definition for ComplexInterval"
    fromInteger n =(fromInteger n::Complex Double,fromInteger n::Complex Double)

instance Fractional ComplexInterval where
    a / b = error "Division of ComplexInterval by ComplexInterval not defined"
    fromRational q = (fromRational q::Complex Double,
                      fromRational q::Complex Double)

instance Interval ComplexInterval where
    type Scalar ComplexInterval = Complex Double
    intersects (z1,w1) (z2,w2) = let
                [x1,x1',x2,x2'] = (map realPart) [z1,w1,z2,w2]
                [y1,y1',y2,y2'] = (map imagPart) [z1,w1,z2,w2]
              in (intersects (x1,x1') (x2,x2'))&&(intersects (y1,y1') (y2,y2'))
    fromScalar(c) = (c,c)
    c `elemI` (z1,w1) = (realPart z1 <= realPart c && realPart c <= realPart w1)
                    && (imagPart z1 <= imagPart c && imagPart c <= imagPart w1)
    c +! (z,w) = (z+c,w+c)
    c -! (z,w) = (c-w,c-z)
    c *! (z,w) = fromScalar(c) * (z,w) --laziness
    c /! (z,w) = error "Division by ComplexInterval not defined"

evaluateI :: (Coefficient a, Interval b, a ~ Scalar b)
             => Polynomial a -> b -> b
evaluateI (a:as) z = a +! (z * (evaluateI as z))
evaluateI [] z = 0

--------------------------------------------------------------------------------
--Finding polynomials with possible roots using tree pruning.
--At the moment just works for the iteration with coefficients in {1,-1},
--but this can easily be extended provided one has an appropriate bound.

bound :: (Coefficient a) => 
         IterCoeffs a -> Degree -> ComplexInterval -> RealInterval
bound coeffs d cI
    |1 `elemI` rI = (0,8) --lousy bound? works for other IterCoeffs??
    |(fst rI) > 1 = (0, maxcoeff * mini (1 /! rI))
    |otherwise = (0, maxcoeff * mini rI)
        where mini rI' = (snd rI')^(d+1) * snd (1 /! (1 -! rI'))
              rI = absI cI
              maxcoeff = maximum (map toAbs coeffs)

--Constructs next level in the tree of polynomials.
nextLevel :: (Coefficient a) => 
              IterCoeffs a -> CoefficientTree a -> CoefficientTree a
nextLevel _ Empty = Empty
nextLevel coeffs (Node c r []) = Node c r (map (\cf ->(Node cf False [])) coeffs)
nextLevel coeffs (Node c r t) = Node c r (map (nextLevel coeffs) t)

--Prunes the tree corresponding to whether values of the iterations of the
--given polynomial, prescribed by the tree, lie within the bound.
--Only checks this at leaves, where it also changes the CanHaveRoot values.
prune :: (Coefficient a) => 
          IterCoeffs a -> ComplexInterval -> Polynomial a 
           -> CoefficientTree a -> CoefficientTree a
prune _ _ _ Empty = Empty
prune coeffs cI p (Node c r [])
    |(0 `elemI` values) = Node c True []
    |((absI $ values ) `intersects` (bound coeffs (length p) cI)) = Node c False []
        --note: length p is the degree of c:p
    |otherwise = Empty
        where values = evaluateI (map toComplex $ p ++ [c]) cI
prune coeffs cI p (Node c r t)
    |(prunedT == []) = Empty
    |otherwise = Node c r prunedT
        where prunedT = filter (/= Empty) $ map (prune coeffs cI (p ++ [c])) t

--Constructs a pruned version of the tree of Littlewood polynomials.
constructTree :: (Coefficient a) => 
             IterCoeffs a -> Degree -> ComplexInterval -> CoefficientTree a
constructTree coeffs 0 _ = (Node 1 False [])
constructTree coeffs d cI = prune coeffs cI [] (nextLevel coeffs $ constructTree coeffs (d-1) cI)

--Returns the polynomials in the tree with CanHaveRoot = True.
getPolynomials :: (Coefficient a) => CoefficientTree a -> [Polynomial a]
getPolynomials Empty = []
getPolynomials (Node c False t) = map (c:) (concatMap getPolynomials t)
getPolynomials (Node c True t)= [[c]] ++ map (c:) (concatMap getPolynomials t)

getAllLeafPolynomials :: (Coefficient a) => CoefficientTree a -> [Polynomial a]
getAllLeafPolynomials Empty = []
getAllLeafPolynomials (Node c _ []) = [[c]]
getAllLeafPolynomials (Node c _ t)= map (c:) (concatMap getAllLeafPolynomials t)

--This gives the possible initial terms for polynomials that might have roots.
canYieldRoots :: (Coefficient a) =>
        IterCoeffs a -> Degree -> ComplexInterval -> [Polynomial a]
canYieldRoots coeffs d cI = getAllLeafPolynomials $ constructTree coeffs d cI

canHaveRoots :: (Coefficient a) =>
        IterCoeffs a -> Degree -> ComplexInterval -> [Polynomial a]
canHaveRoots coeffs d cI = getPolynomials $ constructTree coeffs d cI

--------------------------------------------------------------------------------
--Root finding algorithm using Laguerre's method.

--One iteration of Laguerre's method.
laguerre' :: (Coefficient a) => Polynomial a -> Guess -> Guess
laguerre' q x = x - a
    where p = map toComplex q
          g = (evaluate p' x) / (evaluate p x)
          h = g^2 - (evaluate p'' x) / (evaluate p' x)
          k = sqrt((n-1)*(n*h - g^2))
          s
            | magnitude (g+k) >= magnitude (g-k) = g+k
            | otherwise = g-k
          a = n / s
          n = fromIntegral $ length p -1
          p' = derivative p
          p''= derivative p'

laguerre :: (Coefficient a) => 
        ErrorBound -> Iterations -> Polynomial a -> Guess -> Root
laguerre eps k p g
    | k <= 0 = g
    | magnitude (g' - g) < eps = g' --not a very robust check!
    | otherwise = laguerre eps (k-1) p g'
        where g' = laguerre' p g

--Deflates a polynomial, i.e. divides it by a monic factor.
deflate :: (Coefficient a) => Guess -> Polynomial a -> Polynomial (Complex Double)
deflate g p = tail $ deflate' g as bs
    where [as,bs] = map (map toComplex) [take 1 $ reverse p, drop 1 $ reverse p]
          deflate' z as' bs'
            | bs' == [] = as'
            | otherwise = deflate' z (((head bs')+z*(head as')):as') (tail bs')

findRoots :: (Coefficient a) => 
             ErrorBound -> Iterations -> Polynomial a -> [Root]
findRoots _   _ []     = error "polynomial with no coefficients"
findRoots _   _ [0]    = [0]
findRoots _   _ [_]    = []
findRoots eps k [b, 0] = findRoots eps k [b]
findRoots eps k [b, a] = [ toComplex (-b) / toComplex a]
findRoots eps k p      = z : findRoots eps k (deflate z p2)
    where z = laguerre eps k p2 0
          p2 = reverse $ (dropWhile (==0)) $ reverse p

--------------------------------------------------------------------------------
--Gradients and plotting routines.

showGradient :: Gradient -> String
showGradient = snd

--A black-red-yellow-orange-white gradient, with 0 -> black and h -> white.
warm :: Int -> Gradient
warm h = ((map floor).((map (255*)).warm'), "Warm (height "++show(h)++")")
    where warm' n 
            | n <= 0 = [0,0,0]
            | n >= h = [1,1,1]
            | (n > 0 && n' <= h'/3) = [3*n'/h',0,0]
            | (n' > h'/3 && n' <= 2*h'/3) = [1, 3*n'/h'-1,0]
            | (n'> 2*h'/3 && n < h) = [1,1,3*n'/h'-2]
                where [h',n'] = map fromIntegral [h,n]

--A black-blue-cyan-white gradient.
cold :: Int -> Gradient
cold h = ((map floor).((map (255*)).cold'), "Cold (height "++show(h)++")")
    where cold' n 
            | n <= 0 = [0,0,0]
            | n >= h = [1,1,1]
            | (n > 0 && n' <= h'/3) = [0,0,3*n'/h']
            | (n' > h'/3 && n' <= 2*h'/3) = [0, 3*n'/h'-1,1]
            | (n' > 2*h'/3 && n < h) = [3*n'/h'-2,1,1]
                where [h',n'] = map fromIntegral [h,n]

--Black to white gradient.
grey h = ((map floor).((map (255*)).grey'), "Cold (height "++show(h)++")")
    where grey' n 
            | n <= 0 = [0,0,0]
            | n >= h = [1,1,1]
            | otherwise = [n'/h',n'/h',n'/h']
                where [h',n'] = map fromIntegral [h,n]

--Binary black or white gradient.
binary h = ((map floor).((map (255*)).binary'), "Cold (height "++show(h)++")")
    where binary' n 
            | n < h = [0,0,0]
            | otherwise = [1,1,1]

toCoords :: [Root] -> Resolution -> Center -> Width -> [Pixel]
toCoords roots (rx,ry) c w  = map (\z -> ( floor(realPart z), floor(imagPart z)))
                                    $ map (\z -> (rx'/w :+ 0) * (z-p))
                                    $ filter (\z -> elemI z (p,p')) roots
                            where [rx',ry'] = map (fromIntegral) [rx,ry]
                                  h = w * ry'/rx' 
                                  p = c - ( w/2 :+ h/2)
                                  p'= c + ( w/2 :+ h/2)

--------------------------------------------------------------------------------
--Plotting sets of roots.

{- Useless, and also outdated code.
   Evaluates the polynomials at pixel-wide complex intervals,
   and returns how many of the resulting complex intervals contain 0.
   Produces blocky "spread out" versions of the real images.

colourFunction' :: (Coefficient a) => 
                   [Polynomial a] -> Resolution -> Width -> Center
                   -> Gradient -> Pixel -> Colour
colourFunction' polys (rx,ry) w c (grad,_) (px,py) = col
    where cI = (((realPart c -w/2):+ (imagPart c -h/2)),
                ((realPart c +w/2):+ (imagPart c +h/2)))
          h = w * ry'/rx'
          roots = filter (\pol -> 0 `elemI` (evaluateI pol pI)) polys
          pI = (((px'-0.5)*w/rx':+(py'-0.5)*h/ry'),
                ((px'+0.5)*w/rx':+(py'+0.5)*h/ry')) !+ (fst cI)
          [px',py',rx',ry'] = map (fromIntegral) [px,py,rx,ry]
          col = grad (length roots)
-}

rootList :: (Coefficient a) => 
            [Polynomial a] -> Resolution -> Center -> Width -> [Pixel]
rootList polys (rx,ry) c w  = coordlist
    where rx' = fromIntegral rx
          rootlist= concat $ map (findRoots (0.5*w/rx') 300)
                           $ map (map toComplex) polys
          coordlist = toCoords rootlist (rx,ry) c w

--------------------------------------------------------------------------------
--IFS plotting.

toifs :: (Coefficient a) => IterCoeffs a -> Point -> IFS
toifs coeffs c = (toifs' coeffs c, [c])
    where toifs' coeffs' c' z = map (\cf -> cf*z*c' + 1) (map toComplex coeffs')
        --two normalisations, z -> (cf z c + 1) and z -> (zc + cf)

scaleFactors :: (Coefficient a) => Config a -> [Complex Double]
scaleFactors (Config ic (rx,_) d c eps _) = 
                filterClose (0.2) allscalings
                          -- ^^ random constant, tweaking necessary
    where allscalings = (map $ (negate . recip . (flip evaluate c)) . derivative . (map toComplex)) (canHaveRoots ic d cI)
          cI = c +! ((-eps):+(-eps),eps:+eps)

ifsCheatCounts :: (Coefficient a) => 
                  [Complex Double] -> [Polynomial a] -> Config a -> [Pixel]
ifsCheatCounts scales pols (Config _ res d c w _) = toCoords points res (0:+0) w
    where points' = map (\pol -> (evaluate pol c)) (map (map toComplex) pols)
          points = case scales of
                        [] -> points'
                        otherwise -> (\x -> map (x*) scales) =<< points'
           
ifsIterates :: Iterations -> IFS -> [Complex Double]
ifsIterates 0 (fs,vals) = vals
ifsIterates n (fs,vals) = fs =<< (ifsIterates (n-1) (fs,vals))

ifsCounts :: (Coefficient a) => [Complex Double] -> Config a -> [Pixel]
ifsCounts scales (Config ic res d c w _) = toCoords points res (0:+0) w
    where points' = ifsIterates d (toifs ic c)
          points = case scales of
                        [] -> points'
                        otherwise -> (\x -> map (x*) scales) =<< points'

--------------------------------------------------------------------------------
--Image writing.

toInt :: Colour -> CInt
toInt [r,g,b] = b' + 256*g' + (256^2)*r'
    where [r',g',b'] = map fromIntegral [r,g,b]    
toInt _ = 0

--this possibly flips the image, check!!
writePixel:: GD.Image -> Pixel -> Gradient -> IO()
writePixel image (px,py) (grad,_) = do
    --make this non-monochrome!!
    let col = toInt (grad 1)
    GD.setPixel (px,py) col image

writePixels :: GD.Image -> [Pixel] -> Gradient -> IO()
writePixels image p g
    | null p = return()
    | otherwise = do writePixel image (head p) g
                     writePixels image (tail p) g

writeImage :: FilePath -> [Pixel] -> Resolution -> Gradient -> IO()
writeImage file pixels (rx,ry) (grad,s) = do
    image <- GD.newImage (rx,ry)
    let col = toInt (grad 0)
    GD.fillImage col image
    writePixels image pixels (grad,s)
    GD.savePngFile file image

--------------------------------------------------------------------------------
--Parsing options.

--Basic parsing.
parse :: (Read a) => String -> Maybe a
parse x
    | null y = Nothing
    | otherwise = Just $ fst (head y)
        where y = reads(x)

parseToGradient :: String -> Maybe Gradient
parseToGradient "" = Nothing
parseToGradient s
    | s1 == ["warm"] = Just(warm h)
    | s1 == ["cold"] = Just(cold h)
    | s1 `elem` [["grey"],["gray"]] = Just(grey h)
    | s1 `elem` [["bw"], ["binary"], ["01"], ["10"]] = Just(binary h)
    | otherwise = Nothing
        where s' = words s
              s1 = map (map toLower) $ take 1 s'
              s2 = map parse ( take 1 $ drop 1 s' ) ::[Maybe Int]
              h = case s2 of
                     [Just h'] -> h'
                     otherwise -> 12 --random value

parseToMode :: String -> Maybe Mode
parseToMode s
    | null s = Nothing
    | s1 == ["roots"] = Just Roots
    | s1 == ["ifs"] = Just IFS
    | s1 == ["both"] = Just Both
    | otherwise = Nothing
    where s1 = map (map toLower) (take 1 $ words s)

askYN :: String -> IO(Bool)
askYN s = do
    putStrLn s
    ans' <- getLine
    let ans = map toLower ans'
    case ans of
        "y" -> return True
        "n" -> return False
        _ -> do putStrLn "Sorry? "
                askYN s

askMode :: String -> IO(Mode)
askMode s = do
    putStrLn s
    ans' <- getLine
    let ans = map toLower ans'
    case ans of
        "ifs" -> return IFS
        "roots" -> return Roots
        "both" -> return Both
        _ -> do putStrLn "Sorry? "
                askMode s

parseMConfig :: Coefficient a => [String] -> Maybe (Mode,Config a)
parseMConfig (m:ic:res:d:c:w:g:[]) = (,) <$> mode <*> cfg
    where mode = parseToMode m
          grad = parseToGradient g
          cfg = Config <$> parse ic <*> parse res <*> parse d <*> parse c <*> parse w <*> grad
parseMConfig _ = Nothing

--this should be changed to only accept exception: file doesn't exist...
alwaysError :: IOException -> IO(Maybe a)
alwaysError = \_ -> return Nothing

parseConfigFile :: Coefficient a => IO(Maybe (Mode,Config a))
parseConfigFile = do
    file' <- handle (alwaysError) ((fmap Just) $ (openFile "roots.ini" ReadMode))
    case file' of
        Nothing -> return Nothing
        Just file -> do strings <- (fmap lines) $ hGetContents file
                        return $ parseMConfig (take 7 strings)

parseOptions :: IO((Mode,Config Int))
parseOptions = do
    args' <- fmap parseMConfig getArgs
    cfg' <- parseConfigFile
    ans <- askYN "Valid config file found. Use config file? (y/n) "
    case (args',cfg',ans) of
        (Just args,_,_) -> do putStrLn "Using command line args."
                              return args
        (_,Nothing,_) ->   do putStrLn "No valid config file found. (roots.ini)"
                              askConfig
        (_,Just cfg,True)->return cfg
        otherwise ->       do askConfig

askConfig :: Coefficient a => IO((Mode,Config a))
askConfig = do
    putStrLn "Enter desired mode. (roots/ifs/both). "
    m <- getLine
    putStrLn "Enter desired coefficient set, e.g. [1,-1]."
    ic <- getLine
    putStrLn "Enter desired resolution, e.g. (400,400). "
    res <- getLine
    putStrLn "Enter desired degree. "
    d <- getLine
    putStrLn "Enter desired center, e.g. 0:+(-0.707)."
    c <- getLine
    putStrLn "Enter desired width."
    w <- getLine
    putStrLn "Enter desired gradient. (warm/cold/grey/binary)"
    grad <- getLine
    let mconf'' = [m,ic,res,d,c,w,grad]
    let mconf' = parseMConfig mconf''
    case mconf' of
        Nothing -> do putStrLn "Unable to parse, try again."
                      askConfig
        Just mconf -> return mconf
    
showConfig :: Coefficient a =>  Config a -> IO()
showConfig (Config ic (rx,ry) d c w grad)= do
    putStrLn("Coefficient set is "++show(ic)++".")
    putStrLn("Resolution is "++show(rx)++"x"++show(ry)++".")
    putStrLn("Degree is "++show(d)++".")
    putStrLn("Center is "++show(c)++".")
    let h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
    putStrLn("Width is "++show(w)++", height is "++show(h)++".")
    putStrLn("Gradient is "++showGradient(grad)++".")

--------------------------------------------------------------------------------
--Main routines.

ifsRoutine :: Coefficient a => Config a -> IO()
ifsRoutine (Config ic (rx,ry) d c w g) = do
    let scales = scaleFactors (Config ic (rx,ry) (d+8) c (w/ (fromIntegral rx)) g)  
                    -- bear in mind scaleFactors uses w as an error bound...     
    --                                          
    let ifspixels = ifsCounts scales (Config ic (rx,ry) (d+1) c w g)
    --let h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
    --let cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
    --let pols = canHaveRoots ic d cI
    --let ifspixels = ifsCheatCounts scales pols (Config ic (rx,ry) d c w g)
    --
    
    let ifsfile = "ifs_image.png"
    putStrLn ""
    putStrLn "IFS routine."
    putStrLn "Computing scale factors..."
    putStrLn("Scale factors are: "++(show scales))
    putStrLn "Computing IFS..."
    putStrLn "I'm going to write to file 'ifs_image.png'."
    writeImage ifsfile ifspixels (rx,ry) g
    putStrLn "Done writing to file 'ifs_image.png'. Finished IFS routine."

rootsRoutine :: Coefficient a => Config a -> IO()
rootsRoutine (Config ic (rx,ry) d c w g) = do
    let h = (w* fromIntegral(ry) / (fromIntegral(rx)))::Double
    let cI = c +! ((-w/2) :+ (-h/2),(w/2) :+ (h/2))
    let polys = canHaveRoots ic d cI
    let rootspixels = rootList polys (rx,ry) c w
    let rootsfile = "roots_image.png"
    putStrLn ""
    putStrLn "Roots routine."
    putStrLn "Computing roots."
    putStrLn "I'm going to write to file 'roots_image.png'."
    writeImage rootsfile rootspixels (rx,ry) g 
    putStrLn "Done writing to file 'roots_image.png'. Finished roots routine."

main :: IO()
main = do
    putStrLn "This program produces images of polynomial roots."
    putStrLn ""
    (mode,cfg) <- parseOptions
    putStrLn ""
    showConfig cfg
    case mode of
        Roots -> rootsRoutine cfg
        IFS -> ifsRoutine cfg
        Both -> do rootsRoutine cfg
                   ifsRoutine cfg
    putStrLn ""
    putStrLn "All done here! Press enter to quit."
    getLine
    putStrLn "Bye!"
