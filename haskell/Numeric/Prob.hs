module Numeric.Prob (Prob,
                     complement,
                     fromLogOdds,
                     logOdds,
                     toFloating,
                     LogDouble
                    )
    where

import Numeric.Log
import Numeric.LogDouble
import Data.Ratio
import Data.Floating.Types

{- Odds y = sigmoid(y) is in (0,1), while IOdds y = 1+exp(y) is in
   (1,infinity).  Both increase with y, and the separate endpoint constructors
   keep each value canonical, so constructor ordering is numerical ordering.

   Values above one are needed for intermediate expressions such as 1/2 and
   1-3*p, even though distribution parameters themselves are probabilities.
-}

-- We could keep track of HOW MANY zeros!
-- ... | Zero Int | ...
-- Then we could penalize multiplying by an additional zero.
data Prob = Zero | Odds Double | One | IOdds Double | Infinity deriving (Eq, Ord)

-- Construct a probability from its log odds, retaining exact zero and one at
-- the extended-real endpoints.
fromLogOdds :: Double -> Prob
fromLogOdds y | isNaN y   = error "fromLogOdds: NaN"
              | y == -1/0 = Zero
              | y == 1/0  = One
              | otherwise = Odds y

-- Construct 1+exp(y), retaining canonical one and infinity at the endpoints.
fromLogExcess :: Double -> Prob
fromLogExcess y | isNaN y   = error "fromLogExcess: NaN"
                | y == -1/0 = One
                | y == 1/0  = Infinity
                | otherwise = IOdds y

complement Zero     = One
complement (Odds y) = fromLogOdds (-y)
complement One      = Zero
complement _        = error "complement: not a probability"

fromProb :: (Real a, Pow a) => Prob -> a
fromProb Zero                 = 0
fromProb (Odds y) | y < 0     = let e = expTo y in e / (1 + e)
fromProb (Odds y) | otherwise = 1 / (1 + expTo(-y))
fromProb One                  = 1
fromProb (IOdds y)            = 1 + expTo y
fromProb Infinity             = 1 / 0

toProb :: Double -> Prob
toProb p | isNaN p       = error "toProb: NaN"
         | p < 0         = error "Negative Probability!"
         | p == 0        = Zero
         | p < 1         = fromLogOdds $ log p - log1p (-p)
         | p == 1        = One
         | p > 1         = fromLogExcess $ log (p-1)
         | otherwise     = error ("toProb: unknown number: " ++ show p)

-- Return the extended-real logarithm, including -infinity for exact zero.
logProb :: Prob -> Double
logProb Zero                 = -1/0
logProb (Odds y) | y < 0     = y - log1p (exp y)
                 | otherwise = -log1p (exp (-y))
logProb One                  = 0
logProb (IOdds y)            = log1pexp y
logProb Infinity             = 1/0

-- For z<0, logit(exp(z)) = z-log(1-exp(z)); for z>0, the logarithm
-- of the excess above one is log(exp(z)-1) = z+log(1-exp(-z)).
expToProb :: Double -> Prob
expToProb z | isNaN z   = error "expToProb: NaN"
            | z < 0     = fromLogOdds $ z - log1mexp z
            | z == 0    = One
            | z > 0     = fromLogExcess $ z + log1mexp (-z)
            | otherwise = error ("expToProb: unknown number: " ++ show z)

-- If y12 is only slightly more than zero, we should be able to do better
plus (Odds y1) (Odds y2) | y12 > 0    = toProb (fromProb (Odds y1) + fromProb (Odds y2))
                         | y12 == 0   = One
                         | y12 > (-1) = One - toProb (expm1 (y12) / (1 + exp y1) / (1 + exp y2) )
                         | otherwise      = toProb $ fromProb (Odds y1) + fromProb (Odds y2)
                         where y12 = y1 + y2
plus p@(Odds _) One = fromLogExcess (logProb p)
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
sub One      (Odds y)   = fromLogOdds (-y)
sub p1@(Odds y1) p2@(Odds y2) | y1 >= y2  = p1 * (One - (p2/p1))
sub (IOdds y1) (Odds y2)                  = toProb (fromProb (IOdds y1) - fromProb (Odds y2))
sub (IOdds y1) (IOdds y2)     | y1 == y2  = Zero
                              | y1 >  y2  = toProb (fromProb (IOdds y1) - fromProb (IOdds y2))
sub _         _ = error "Negative probability"


mul (Odds y1) (Odds y2) | y1 > y2   = fromLogOdds $ y2 - log1p( exp(y2-y1) + exp(-y1) )
                        | otherwise = fromLogOdds $ y1 - log1p( exp(y1-y2) + exp(-y2) )
-- For x=sigmoid(y1)*(1+exp(y2)), x is below or above one according
-- to s=y1+y2; the two formulas are respectively logit(x) and log(x-1).
mul (Odds y1) (IOdds y2) | s == 0    = One
                         | s < 0     = fromLogOdds $ y1 + log1pexp(y2) - log1mexp(s)
                         | otherwise = fromLogExcess $ logexpm1(s) - log1pexp(y1)
                         where s = y1+y2
mul (IOdds y1) (Odds y2) = mul (Odds y2) (IOdds y1)
-- Since recip(IOdds y) = Odds(-y), invert the product of the reciprocals.
mul (IOdds y1) (IOdds y2) = recip $ mul (Odds (-y1)) (Odds (-y2))
mul Zero      Infinity  = error "0 * Inf is undefined"
mul Infinity  Zero      = error "Inf * 0 is undefined"
mul Zero      x         = Zero
mul x         Zero      = Zero
mul One       x         = x
mul x         One       = x
mul Infinity  x         = Infinity
mul x         Infinity  = Infinity

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
                  | otherwise = fromLogExcess $ integerToLogExcess x

instance Real Prob where
    toRational Zero = 0 % 1
    toRational (Odds y) = toRational $ (fromProb (Odds y) :: Double)
    toRational One  = 1 % 1
    toRational (IOdds y) = toRational $ (fromProb (IOdds y) :: Double)
    toRational Infinity = 1 % 0

instance Fractional Prob where
    recip Zero      = Infinity
    recip (Odds y)  = fromLogExcess (-y)
    recip One       = One
    recip (IOdds y) = fromLogOdds (-y)
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
powProb (Odds y)  t | y > 30    = fromLogOdds $ y - log t
                    | y < 0     = fromLogOdds $ t*y - log ( (1+exp(y))**t - exp(t*y) )
                    | otherwise = fromLogOdds $ negate $ log $ expm1 ( t * log1p ( exp (-y)) )
powProb (IOdds y) t             = recip $ pow (fromLogOdds (-y)) t

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

foreign import ecall "Real:" integerToLogExcess :: Integer -> Double
