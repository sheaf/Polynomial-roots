{-# LANGUAGE TypeFamilies, TypeSynonymInstances,
             FlexibleContexts, FlexibleInstances #-}
module Interval where

import Types
import Data.Maybe
import Control.Applicative

--------------------------------------------------------------------------------
--Interval arithmetic.
--Inexact operations (which introduce spurious results) are indicated.

class (Fractional a, Fractional (Scalar a)) => Interval a where
    type Scalar a :: *
    intersects :: a -> a -> Bool
    intersect :: a -> a -> Maybe a
    intersects i j = not $ isNothing $ intersect i j
    elemI :: Scalar a -> a -> Bool
    (+!) :: Scalar a -> a -> a
    c +! i = fromScalar(c) + i
    (!+) :: a -> Scalar a -> a
    (!+) = flip (+!)
    (-!) :: Scalar a -> a -> a
    c -! i = fromScalar(c) - i
    (!-) :: a -> Scalar a -> a
    (!-) = (!+).negate
    (*!) :: Scalar a -> a -> a
    c *! i = fromScalar(c) * i
    (!*) :: a -> Scalar a -> a
    (!*) = flip (*!)
    (/!) :: Scalar a -> a -> a
    c /! i = fromScalar(c) / i
    (!/) :: a -> Scalar a -> a
    x !/ y = x !* recip(y)
    fromScalar :: Scalar a -> a

--------------------------------------------------------------------------------
--Real intervals, [a,b] = { x : a <= x <= b}.

type RealInterval = (Double,Double)

instance Num RealInterval where
    (x1,y1) + (x2,y2) = (x1+x2,y1+y2)
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
    intersect (x1,y1) (x2,y2)
        | (x1 <= x2 && y1 >= x2) = Just (x2, minimum[y1,y2])
        | (x1 >= x2 && x1 <= y2) = Just (x1, minimum[y1,y2])
        | otherwise = Nothing
    a `elemI` (x1,y1) = (a >= x1 && a <= y1)
    a +! (x1,y1) = (x1+a,y1+a)
    a -! (x1,y1) = (a-y1,a-x1)
    a *! (x1,y1)
        | a > 0 = (a*x1,a*y1)
        | a < 0 = (a*y1,a*x1)
        | a == 0 = (0,0)
    a /! (x1,y1) = a *! (recip(y1), recip(x1))
    fromScalar a = (a,a)
    
--------------------------------------------------------------------------------
--Rectangular complex intervals, given by lower left and upper right corners.

type ComplexInterval = (Complex Double,Complex Double)

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
absI (z,w) = (minabsI(z,w),maxabsI(z,w))

instance Num ComplexInterval where
    (z1,w1) + (z2,w2) = (z1+z2,w1+w2)
    (z1,w1) * (z2,w2) = let --inexact.
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
    intersect (z1,w1) (z2,w2) = let
                [x1,x1',x2,x2'] = (map realPart) [z1,w1,z2,w2]
                [y1,y1',y2,y2'] = (map imagPart) [z1,w1,z2,w2]
                ix = (intersect (x1,x1') (x2,x2'))
                iy = (intersect (y1,y1') (y2,y2')) in
                (\(x,x') (y,y') -> (x:+y, x':+y')) <$> ix <*> iy 
    fromScalar(c) = (c,c)
    c `elemI` (z1,w1) = (realPart z1 <= realPart c && realPart c <= realPart w1)
                    && (imagPart z1 <= imagPart c && imagPart c <= imagPart w1)
    c +! (z,w) = (z+c,w+c)
    c -! (z,w) = (c-w,c-z)
    
--------------------------------------------------------------------------------
--Disk complex intervals, given by center and radius.

type Disk = (Complex Double, Double)

instance Num Disk where
    (c1,r1) + (c2,r2) = (c1+c2,r1+r2)
    negate (c,r) = (negate c, r)
    (c1,r1) * (c2,r2) = ( c1*c2, magnitude(c1) * r2 + magnitude(c2)*r1 + r1*r2)
    -- ^^ inexact; try exact version too
    abs (c,r) = (realToFrac $ magnitude c + r, 0)
    signum = error "No signum definition for Disk"
    fromInteger n = (fromInteger n::Complex Double, 0)
    
instance Fractional Disk where
    recip (c, r) = (c', r')
        where m = (magnitude c)^2 - r^2
              c' = conjugate c / realToFrac m
              r' = r / m
    fromRational q = (fromRational q::Complex Double, 0)

instance Interval Disk where
    type Scalar Disk = Complex Double
    intersects (c1,r1) (c2,r2) = (magnitude (c1-c2)) <= (r1+r2)
    intersect = error "Intersection of disks is not a disk"
    z `elemI` (c,r) = magnitude (z - c) <= r
    fromScalar z = (z,0)
    
--------------------------------------------------------------------------------

--Gives the coefficients of the taylor expansion of p centered at c.
taylor :: (Coefficient a, Fractional a) => 
          Polynomial a -> a -> Polynomial a
taylor p c = zipWith (/) derivs facts 
            where derivs = map (flip evaluate c) $ takeWhile (not.null) $ iterate derivative p
                  facts = map fromIntegral $ scanl (\x y -> x*y) 1 [1..]

--Evaluation of polynomials on intervals, using Horner scheme.
evaluateI :: (Coefficient a, Interval b, a ~ Scalar b)
             => Polynomial a -> b -> b
evaluateI (a:as) z = a +! (z * (evaluateI as z))
evaluateI [] z = 0

--Evaluation of polynomials on disks, always producing less spurious results.
--That is, evaluateD p d is always a subset of evaluateI p d
evaluateD :: (a ~ Complex Double) => Polynomial a -> Disk -> Disk
evaluateD p (c,r) = (c',r')
    where c' = evaluate p c
          r' = evaluate (map magnitude $ 0 : (drop 1 (taylor p c))) r

