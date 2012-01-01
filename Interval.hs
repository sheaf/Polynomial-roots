{-# LANGUAGE TypeFamilies, TypeSynonymInstances,
             FlexibleContexts, FlexibleInstances #-}
module Interval where

import Types

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
