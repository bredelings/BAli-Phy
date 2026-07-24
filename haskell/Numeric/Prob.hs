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

-- For p_i=sigmoid(y_i), their sum has numerator exp(y1)+exp(y2)+2*exp(y1+y2).
-- Its distance from one has numerator 1-exp(y1+y2), with the sign reversed above one.
plus (Odds y1) (Odds y2) | s < 0     = fromLogOdds $ logNumerator - log1mexp(s)
                         | s == 0    = One
                         | otherwise = fromLogExcess $
                                           logexpm1(s) - log1pexp(y1) - log1pexp(y2)
                         where s = y1+y2
                               logNumerator = logsum y1 $ logsum y2 (log 2+s)
plus p@(Odds _) One = fromLogExcess (logProb p)
-- Adding Odds y1 to 1+exp(y2) leaves an excess of sigmoid(y1)+exp(y2).
plus p@(Odds _) (IOdds y2) = fromLogExcess $ logsum (logProb p) y2
plus (IOdds y1) (Odds y2) = plus (Odds y2) (IOdds y1)
plus One        (Odds y)  = plus (Odds y) One
plus One One = fromLogExcess 0
plus One (IOdds y) = fromLogExcess $ log1pexp y
plus (IOdds y) One = plus One (IOdds y)
-- The excess of (1+exp(y1))+(1+exp(y2)) above one is 1+exp(y1)+exp(y2).
plus (IOdds y1) (IOdds y2) = fromLogExcess $ logsum 0 (logsum y1 y2)
plus Zero x = x
plus x Zero = x
plus Infinity x = Infinity
plus x Infinity = Infinity


sub x Zero = x
sub Infinity Infinity = error "Inf - Inf is undefined"
sub Infinity _ = Infinity
sub One One = Zero
sub One (Odds y) = fromLogOdds (-y)
-- For p1>=p2, p1-p2 = p1*(1-p2/p1), keeping both factors in [0,1].
sub p1@(Odds y1) p2@(Odds y2) | y1 >= y2  = p1 * (One - (p2/p1))
-- (1+exp(y1))-sigmoid(y2) = exp(y1)+sigmoid(-y2), avoiding cancellation near one.
sub (IOdds y1) (Odds y2) = plus (expToProb y1) (fromLogOdds (-y2))
sub (IOdds y) One = expToProb y
-- The ones cancel, leaving exp(y1)-exp(y2) in a stable log-difference form.
sub (IOdds y1) (IOdds y2) | y1 == y2 = Zero
                          | y1 > y2  = expToProb $ y1 + log1mexp(y2-y1)
sub _ _ = error "Negative probability"


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


-- For p=sigmoid(y), write p=exp(-a), where a=log(1+exp(-y)).  Then
-- logit(p^t)=-log(exp(t*a)-1); the log-domain branch retains t*a when it underflows.
powProb :: Prob -> Double -> Prob
powProb _ t | isNaN t = error "pow: NaN exponent"
powProb _ 0 = One
powProb p t | t < 0 = recip $ powProb p (-t)
powProb One _ = One
powProb Zero _ = Zero
powProb Infinity _ = Infinity
-- For y>40, log(log(1+exp(-y))) rounds to -y at Double precision, so computing
-- log(t*a) as log(t)-y also avoids underflow in a.
powProb (Odds y) t
    | y > 40 =
        let logQ = log t-y
        in if logQ < -40
           then fromLogOdds (-logQ)
           else fromLogOdds $ negate $ logexpm1 $ exp logQ
    | q == 0 = fromLogOdds $ negate (log t+log a)
    | otherwise = fromLogOdds $ negate $ logexpm1 q
    where a = log1pexp (-y)
          q = t*a
powProb (IOdds y) t = recip $ powProb (fromLogOdds (-y)) t

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
