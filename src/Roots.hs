{-# LANGUAGE ScopedTypeVariables #-}

module Roots where

-- base
import Control.Monad
  ( unless, when )
import Control.Monad.ST
  ( ST, runST )
import Data.Complex
  ( Complex(..), conjugate, magnitude, realPart, imagPart )
import Data.Functor.Compose
  ( Compose(..) )
import Data.Maybe
  ( mapMaybe )

-- containers
import Data.Tree
  ( Forest )

-- deepseq
import Control.DeepSeq
  ( NFData, force )

-- primitive
import Control.Monad.Primitive
  ( PrimMonad(PrimState) )
import Data.Primitive.PrimArray
  ( PrimArray, MutablePrimArray
  , primArrayFromList
  , getSizeofMutablePrimArray, sizeofPrimArray
  , unsafeThawPrimArray, cloneMutablePrimArray
  , shrinkMutablePrimArray, readPrimArray, writePrimArray
  )
import Data.Primitive.Types
  ( Prim )

-- prim-instances
import Data.Primitive.Instances
  () -- instance Prim a => Prim ( Complex a )

-- polynomial-roots
import Interval
import Polynomials
  ( trim )
import Types
import Trees

--------------------------------------------------------------------------------
--Bound used for pruning trees of polynomials.

-- | Bounds the contribution of the remaining terms past an initial segment.
-- This is used to prune trees of polynomials.
bound :: (Coefficient a) => IterCoeffs a -> ComplexInterval -> RealBound
bound cfs cI d
    |fst rI > 1 = maxcoeff * mini (1 /! rI)
    |otherwise = maxcoeff * mini rI
        where mini rI' = (snd rI')^(d+1) * snd (1 /! (1 -! rI'))
              rI = absI cI
              maxcoeff = maximum (map toAbs cfs)

-- | Gives the possible initial segments of polynomials that can have roots.
canYieldRoots :: (Coefficient a) =>
              IterCoeffs a -> Degree -> ComplexInterval -> [Polynomial a]
canYieldRoots cfs d cI = concatMap getAllLeafPolynomials
                       $ constructForest d cfs (bound cfs cI) cI

-- | Returns all polynomials up to the given degree which can have roots
-- in the specified region.
canHaveRoots :: (Coefficient a) =>
             IterCoeffs a -> Degree -> ComplexInterval -> Forest (Polynomial a)
canHaveRoots cfs d cI = toPolyForest [] $ constructForest d cfs (bound cfs cI) cI

--------------------------------------------------------------------------------
-- Root finding.

-- | Root finding methods.
findRoots, findRoots' :: Coefficient a => Polynomial a -> [ Root ]
findRoots = findRoots' . trim
findRoots' []    = []
findRoots' [_]   = []
findRoots' [b,a] = [ toComplex -b / toComplex a ]
findRoots' p
  | Just pr <- traverse toReal p
  = rootsR 1e-10 40 ( reverse pr )
  | otherwise
  = rootsC 1e-10 40 ( reverse $ map toComplex p )

--------------------------------------------------------------------------------
-- Plotting sets of roots.

-- | Returns the forest of polynomials that can have roots in a region.
getPolys :: (Coefficient a) => Config c a -> Forest (Polynomial a)
getPolys (Config ic (rx, ry) d c w _ _) = canHaveRoots ic d cI
  where h  = w * fromIntegral ry / fromIntegral rx
        cI = c +! ((-w/2) :+ (-h/2), (w/2) :+ (h/2))

-- | Returns all possible roots that can occur in a region, up to a given degree.
getRoots :: (Coefficient a) => ((Polynomial a -> [Root]) -> (Polynomial a -> b))
         -> Config c a -> Forest b
getRoots q cfg = getCompose . fmap (q findRoots) . Compose $ getPolys cfg


--------------------------------------------------------------------------------
-- Root finding using Laguerre's method

-- | Find real roots of a polynomial with real coefficients.
--
-- Coefficients are given in order of decreasing degree, e.g.:
--  x² + 7 is given by [ 1, 0, 7 ].
realRoots :: forall a. ( RealFloat a, Prim a, NFData a ) => Int -> [ a ] -> [ a ]
realRoots maxIters coeffs = mapMaybe isReal ( rootsR epsilon maxIters coeffs )
  where
    isReal :: Complex a -> Maybe a
    isReal ( a :+ b )
      | abs b < epsilon = Just a
      | otherwise       = Nothing

-- | Compute all roots of a polynomial with real coefficients using Laguerre's method and (forward) deflation.
--
-- Polynomial coefficients are given in order of descending degree (e.g. constant coefficient last).
--
-- N.B. The forward deflation process is only guaranteed to be numerically stable
-- if Laguerre's method finds roots in increasing order of magnitude
-- (which this function attempts to do).
rootsR :: forall a. ( RealFloat a, Prim a, NFData a )
      => a -> Int -> [ a ] -> [ Complex a ]
rootsR eps maxIters coeffs = runST do
  let
    coeffPrimArray :: PrimArray a
    coeffPrimArray = primArrayFromList coeffs
    sz :: Int
    sz = sizeofPrimArray coeffPrimArray
  ( p :: MutablePrimArray s a ) <- unsafeThawPrimArray coeffPrimArray
  let
    go :: Int -> [ Complex a ] -> ST s [ Complex a ]
    go !i rs = do
      -- Start at 0, attempting to find the root with smallest absolute value.
      -- This improves numerical stability of the forward deflation process.
      !r <- force <$> laguerreR eps maxIters p 0
      if i <= 2
      then pure ( r : rs )
      else
        -- real root
        if abs ( imagPart r ) < epsilon
        then do
          deflate ( realPart r ) p
          go ( i - 1 ) ( r : rs )
        else do
          deflateConjugatePair r p
          go ( i - 2 ) ( r : conjugate r : rs )
  go sz []

rootsC :: forall a. ( RealFloat a, Prim a, NFData a )
       => a -> Int -> [ Complex a ] -> [ Complex a ]
rootsC eps maxIters coeffs = runST do
  let
    coeffPrimArray :: PrimArray ( Complex a )
    coeffPrimArray = primArrayFromList coeffs
    sz :: Int
    sz = sizeofPrimArray coeffPrimArray
  ( p :: MutablePrimArray s ( Complex a ) ) <- unsafeThawPrimArray coeffPrimArray
  let
    go :: Int -> [ Complex a ] -> ST s [ Complex a ]
    go !i rs = do
      -- Start at 0, attempting to find the root with smallest absolute value.
      -- This improves numerical stability of the forward deflation process.
      !r <- force <$> laguerreC eps maxIters p 0
      if i <= 2
      then pure ( r : rs )
      else do
        deflate r  p
        go ( i - 1 ) ( r : rs )
  go sz []

-- | Forward deflation of a polynomial by a root: factors out the root.
--
-- Mutates the input polynomial.
deflate :: forall a m s. ( Num a, Prim a, PrimMonad m, s ~ PrimState m )
        => a -> MutablePrimArray s a -> m ()
deflate r p = do
  deg <- subtract 1 <$> getSizeofMutablePrimArray p
  when ( deg >= 2 ) do
    shrinkMutablePrimArray p deg
    let
      go :: a -> Int -> m ()
      go !b !i = unless ( i >= deg ) do
        ai <- readPrimArray p i
        let
          bi :: a
          !bi = ai + r * b
        writePrimArray p i bi
        go bi ( i + 1 )
    a0 <- readPrimArray p 0
    go a0 1

-- | Forward deflation of a polynomial with real coefficients by a pair of complex-conjugate roots.
--
-- Mutates the input polynomial.
deflateConjugatePair :: forall a m s. ( Num a, Prim a, PrimMonad m, s ~ PrimState m )
                     => Complex a -> MutablePrimArray s a -> m ()
deflateConjugatePair ( x :+ y ) p = do
  deg <- subtract 1 <$> getSizeofMutablePrimArray p
  when ( deg >= 3 ) do
    shrinkMutablePrimArray p ( deg - 1 )
    let
      c1, c2 :: a
      !c1 = 2 * x
      !c2 = x * x + y * y
    a0 <- readPrimArray p 0
    a1 <- readPrimArray p 1
    let
      b1 :: a
      !b1 = a1 + c1 * a0
    writePrimArray p 1 b1
    let
      go :: a -> a -> Int -> m ()
      go !b !b' !i = unless ( i >= deg - 1 ) do
        ai <- readPrimArray p i
        let
          bi :: a
          !bi = ai + c1 * b - c2 * b'
        writePrimArray p i bi
        go bi b ( i + 1 )
    go b1 a0 2

-- | Laguerre's method.
--
-- Does not perform any mutation.
laguerreR
  :: forall a m s
  .  ( RealFloat a, Prim a, PrimMonad m, s ~ PrimState m )
  => a                    -- ^ error tolerance
  -> Int                  -- ^ max number of iterations
  -> MutablePrimArray s a -- ^ polynomial
  -> Complex a            -- ^ initial point
  -> m ( Complex a )
laguerreR eps maxIters p x0 = do
  p'  <- derivative p
  p'' <- derivative p'
  let
    go :: Int -> Complex a -> m ( Complex a )
    go !iterationsLeft !x = do
      x' <- laguerreStepR eps p p' p'' x
      if iterationsLeft <= 1 || magnitude ( x' - x ) < eps
      then pure x'
      else go ( iterationsLeft - 1 ) x'
  go maxIters x0

-- | Laguerre's method.
--
-- Does not perform any mutation.
laguerreC
  :: forall a m s
  .  ( RealFloat a, Prim a, PrimMonad m, s ~ PrimState m )
  => a                    -- ^ error tolerance
  -> Int                  -- ^ max number of iterations
  -> MutablePrimArray s ( Complex a ) -- ^ polynomial
  -> Complex a            -- ^ initial point
  -> m ( Complex a )
laguerreC eps maxIters p x0 = do
  p'  <- derivative p
  p'' <- derivative p'
  let
    go :: Int -> Complex a -> m ( Complex a )
    go !iterationsLeft !x = do
      x' <- laguerreStepC eps p p' p'' x
      if iterationsLeft <= 1 || magnitude ( x' - x ) < eps
      then pure x'
      else go ( iterationsLeft - 1 ) x'
  go maxIters x0

-- | Take a single step in Laguerre's method.
--
-- Does not perform any mutation.
laguerreStepR
  :: forall a m s
  .  ( RealFloat a, Prim a, PrimMonad m, s ~ PrimState m )
  => a                    -- ^ error tolerance
  -> MutablePrimArray s a -- ^ polynomial
  -> MutablePrimArray s a -- ^ first derivative of polynomial
  -> MutablePrimArray s a -- ^ second derivative of polynomial
  -> Complex a            -- ^ initial point
  -> m ( Complex a )
laguerreStepR eps p p' p'' x = do
  n  <- fromIntegral @_ @a <$> getSizeofMutablePrimArray p
  px <- evalR p x
  if magnitude px < eps
  then pure x
  else do
    p'x  <- evalR p'  x
    p''x <- evalR p'' x
    let
      g     = p'x / px
      g²    = g * g
      h     = g² - p''x / px
      delta = sqrt $ ( n - 1 ) *: ( n *: h - g² )
      gp    = g + delta
      gm    = g - delta
      denom
        | magnitude gm > magnitude gp
        = recip gm
        | otherwise
        = recip gp
    pure $ x - n *: denom

  where
    (*:) :: a -> Complex a -> Complex a
    r *: (u :+ v) = ( r * u ) :+ ( r * v )

-- | Take a single step in Laguerre's method.
--
-- Does not perform any mutation.
laguerreStepC
  :: forall a m s
  .  ( RealFloat a, Prim a, PrimMonad m, s ~ PrimState m )
  => a                                -- ^ error tolerance
  -> MutablePrimArray s ( Complex a ) -- ^ polynomial
  -> MutablePrimArray s ( Complex a ) -- ^ first derivative of polynomial
  -> MutablePrimArray s ( Complex a ) -- ^ second derivative of polynomial
  -> Complex a                        -- ^ initial point
  -> m ( Complex a )
laguerreStepC eps p p' p'' x = do
  n  <- fromIntegral @_ @a <$> getSizeofMutablePrimArray p
  px <- evalC p x
  if magnitude px < eps
  then pure x
  else do
    p'x  <- evalC p'  x
    p''x <- evalC p'' x
    let
      g     = p'x / px
      g²    = g * g
      h     = g² - p''x / px
      delta = sqrt $ ( n - 1 ) *: ( n *: h - g² )
      gp    = g + delta
      gm    = g - delta
      denom
        | magnitude gm > magnitude gp
        = recip gm
        | otherwise
        = recip gp
    pure $ x - n *: denom

  where
    (*:) :: a -> Complex a -> Complex a
    r *: (u :+ v) = ( r * u ) :+ ( r * v )

-- | Evaluate a polynomial with real coefficients at a complex number.
--
-- Does not perform any mutation.
evalR
  :: forall a m s
  . ( RealFloat a, Prim a, PrimMonad m, s ~ PrimState m )
  => MutablePrimArray s a -> Complex a -> m ( Complex a )
evalR p x = do
  n <- getSizeofMutablePrimArray p
  let
    go :: Complex a -> Int -> m ( Complex a )
    go !a !i
      | i >= n
      = pure a
      | otherwise
      = do
        b <- readPrimArray p i
        go ( ( b :+ 0 ) + x * a ) ( i + 1 )
  an <- readPrimArray p 0
  go ( an :+ 0 ) 1

-- | Evaluate a polynomial with real coefficients at a complex number.
--
-- Does not perform any mutation.
evalC
  :: forall a m s
  . ( RealFloat a, Prim a, PrimMonad m, s ~ PrimState m )
  => MutablePrimArray s ( Complex a ) -> Complex a -> m ( Complex a )
evalC p x = do
  n <- getSizeofMutablePrimArray p
  let
    go :: Complex a -> Int -> m ( Complex a )
    go !a !i
      | i >= n
      = pure a
      | otherwise
      = do
        b <- readPrimArray p i
        go ( b + x * a ) ( i + 1 )
  an <- readPrimArray p 0
  go an 1

-- | Derivative of a polynomial.
--
-- Does not mutate its argument.
derivative
  :: forall a m s
  . ( Num a, Prim a, PrimMonad m, s ~ PrimState m )
  => MutablePrimArray s a -> m ( MutablePrimArray s a )
derivative p = do
  deg <- subtract 1 <$> getSizeofMutablePrimArray p
  p' <- cloneMutablePrimArray p 0 deg
  let
    go :: Int -> m ()
    go !i = unless ( i >= deg - 1 ) do
      a <- readPrimArray p' i
      writePrimArray p' i ( a * fromIntegral ( deg - i ) )
      go ( i + 1 )
  go 0
  pure p'

{-# SPECIALISE epsilon :: Float  #-}
{-# SPECIALISE epsilon :: Double #-}
epsilon :: forall r. RealFloat r => r
epsilon = encodeFloat 1 ( 5 - floatDigits ( 0 :: r ) )

nearZero :: RealFloat r => r -> Bool
nearZero x = abs x < epsilon
