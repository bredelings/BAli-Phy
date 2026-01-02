module Numeric.Prob (Prob (..),
                     complement,
                     logOdds,
                     toFloating,
                     LogDouble
                    )
    where

import Numeric.Log
import Numeric.LogDouble
import Data.Ratio
import Data.Floating.Types

{- We need to handle numbers that are > 1 in the following cases:
     1/2   (2 > 1)
     1-3*p (3 > 1)
   So, we assume that IOdds z = 1 / Odds z -}

-- We could keep track of HOW MANY zeros!
-- ... | Zero Int | ...
-- Then we could penalize multiplying by an additional zero.
data Prob = Zero | Odds Double | One | IOdds Double | Infinity

complement Zero     = One
complement (Odds y) = (Odds (-y))
complement One      = Zero
complement _        = error "complement: not a probability"

fromProb :: (Real a, Pow a) => Prob -> a
fromProb Zero                 = 0
fromProb (Odds y) | y < 0     = let e = expTo y in e / (1 + e)
fromProb (Odds y) | otherwise = 1 / (1 + expTo(-y))
fromProb One                  = 1
fromProb (IOdds y)| y < 0     = let e = expTo y in (1 + e)/e
                  | otherwise = (1 + expTo(-y))
fromProb Infinity             = 1 / 0

toProb :: Double -> Prob
toProb p | p < 0         = error "Negative Probability!"
         | p == 0        = Zero
         | p < 1         = Odds $ log $ p / (1-p)
         | p == 1        = One
         | p > 1         = recip $ toProb (1/p)
         | isInfinite p  = Infinity
         | isNaN p       = error "toProb: NaN"
         | otherwise     = error ("toProb: unknown number: " ++ show p)

-- Only defined on non-zero probabilities.
logProb :: Prob -> Double
logProb Zero                 = error "Probability: log(0)"
logProb (Odds y) | y < 0     = y - log1p (exp y)
                 | otherwise = -log1p (exp (-y))
logProb One                  = 0
logProb (IOdds y)            = -logProb (Odds y)
logProb Infinity             = 1/0

expToProb :: Double -> Prob
expToProb z | z < 0     = Odds $ z - log1p (-exp z)
            | z == 0    = One
            | z > 0     = let Odds z2 = expToProb (-z) in IOdds z2

-- If y12 is only slightly more than zero, we should be able to do better
plus (Odds y1) (Odds y2) | y12 > 0    = toProb (fromProb (Odds y1) + fromProb (Odds y2))
                         | y12 == 0   = One
                         | y12 > (-1) = One - toProb (expm1 (y12) / (1 + exp y1) / (1 + exp y2) )
                         | otherwise      = toProb $ fromProb (Odds y1) + fromProb (Odds y2)
                         where y12 = y1 + y2
plus (Odds y)   One      | y < 0     = IOdds $ log1p(exp(y)) - y
                         | otherwise = IOdds $ log1p(exp(-y))
plus (IOdds y1) (Odds y2) = plus (Odds y2) (IOdds y1)
plus One        (Odds y)  = plus (Odds y) One
plus Zero      x        = x
plus x         Zero     = x
plus Infinity  x        = Infinity
plus x         Infinity = Infinity
plus x1         x2        = toProb (fromProb x1 + fromProb x2)


sub x        Zero        = x
sub Infinity Infinity    = error "Inf - Inf is undefined"
sub Infinity _           = Infinity
sub One      One         = Zero
sub One      (Odds y)   = Odds (-y)
sub p1@(Odds y1) p2@(Odds y2) | y1 >= y2  = p1 * (One - (p2/p1))
sub (IOdds y1) (Odds y2)                  = toProb (fromProb (IOdds y1) - fromProb (Odds y2))
sub (IOdds y1) (IOdds y2)     | y1 == y2  = Zero
                              | y1 <  y2  = toProb (fromProb (IOdds y1) - fromProb (IOdds y2))
sub _         _ = error "Negative probability"


mul (Odds y1) (Odds y2) | y1 > y2   = Odds $ y2 - log1p( exp(y2-y1) + exp(-y1) )
                        | otherwise = Odds $ y1 - log1p( exp(y1-y2) + exp(-y2) )
-- Note that as y1 and y2 increase, both tend towards 1, so whichever is SMALLER dominates.
mul (Odds y1) (IOdds y2) | y1 == y2   = One
                         | y1 < y2    = Odds $ y1 + log1pexp(-y2) - log1mexp(y1-y2)
                         | otherwise  = IOdds $ y2 + log1pexp(-y1) - log1mexp(y2-y1)
mul (IOdds y1) (Odds y2) = mul (Odds y2) (IOdds y1)
mul (IOdds y1) (IOdds y2) = recip $ mul (Odds y1) (Odds y2)
mul Zero      Infinity  = error "0 * Inf is undefined"
mul Infinity  Zero      = error "Inf * 0 is undefined"
mul Zero      x         = Zero
mul x         Zero      = Zero
mul One       x         = x
mul x         One       = x
mul Infinity  x         = Infinity
mul x         Infinity  = Infinity

instance Eq Prob where
    (Odds y1)  == (Odds y2)  = y1 == y2
    (IOdds y1) == (IOdds y2) = y1 == y2
    x          == y          = pord x == pord y

pord Zero      = 0
pord (Odds _)  = 1
pord One       = 2
pord (IOdds _) = 3
pord Infinity  = 4

instance Ord Prob where
    (Odds y1) < (Odds y2) = y1 < y2
    (IOdds y1) < (IOdds y2) = y1 > y2
    x < y  = pord x < pord y

instance Num Prob where
    (+) = plus
    (-) = sub
    (*) = mul
    abs = id
    negate = error "Can't negate a probability"
    signum Zero = 0
    signum _    = 1
    fromInteger 0 = Zero
    fromInteger 1 = One
    fromInteger x | x < 0     = error "Negative Probability!"
                  | otherwise = IOdds $ integerToInvLogOdds x

instance Real Prob where
    toRational Zero = 0 % 1
    toRational (Odds y) = toRational $ (fromProb (Odds y) :: Double)
    toRational One  = 1 % 1
    toRational (IOdds y) = toRational $ (fromProb (IOdds y) :: Double)
    toRational Infinity = 1 % 0

instance Fractional Prob where
    recip Zero      = Infinity
    recip (Odds y)  = IOdds y
    recip One       = One
    recip (IOdds y) = Odds y
    recip Infinity  = Zero

instance Show Prob where
    show Zero = "0"
    show (Odds y) = show $ (fromProb (Odds y) :: Double)
    show One = "1"
    show (IOdds y) = show $ (fromProb (IOdds y) :: Double)
    show Infinity = "Inf"


logOdds (Odds y) = y
logOdds Zero = -1/0
logOdds One = 1/0
logOdds (IOdds _) = error "logOdds(x) where x > 1"
logOdds Infinity = error "logOdds Infinity"


-- t regions: (-Inf,-1], (-1, 0), (0,1), [1,Inf)
-- y regions for t > 1: (-Inf,0),[0,30),[30,Inf)
--  maybe for t in (0,1) we need different y regions?

-- we need to avoid exp(t*y) when t*y is large and positive.

-- It would be nice if we could write y ** t, but t is not a Prob
powProb :: Prob -> Double -> Prob
powProb One       t             = One
powProb y         t | t == 0    = One
powProb Zero      t | t < 0     = Infinity
                    | otherwise = Zero      -- t > 0
powProb Infinity  t | t < 0     = Zero
                    | otherwise = Infinity  -- t > 0
-- This probably works best for t > 1, when exp(tx) < exp(x).
powProb (Odds y)  t | y > 30    = Odds $ y - log t
                    | y < 0     = Odds $ t*y - log ( (1+exp(y))**t - exp(t*y) )
                    | otherwise = Odds $ negate $ log $ expm1 ( t * log1p ( exp (-y)) )
powProb (IOdds y) t             = recip $ pow (Odds y) t

instance Pow Prob where
    ln = logProb
    pow = powProb
    expTo = expToProb

instance FloatConvert Prob Double where
    toFloating = fromProb

instance FloatConvert Prob LogDouble where
    toFloating = fromProb

instance FloatConvert LogDouble Prob where
    toFloating = expTo . ln

instance FloatConvert Double Prob where
    toFloating = toProb

-- Handle Int, Integer, and anything else that we can get a double from.
instance {-# OVERLAPPABLE #-} FloatConvert a Double => FloatConvert a Prob where
    toFloating i = toFloating (toFloating i :: Double)

foreign import ecall "Real:" integerToInvLogOdds :: Integer -> Double
