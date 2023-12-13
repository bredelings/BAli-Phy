module Data.Nat (
   Nat(..),
   toNatural,
   fromNatural,
   cata,
   -- explicitShow
   -- explicitShowsPrec
    nat0, nat1, nat2, nat3, nat4, nat5, nat6, nat7, nat8, nat9
                ) where

import Numeric.Natural

data Nat = Z | S Nat


instance Eq Nat where
    Z     == Z      = True
    (S m) == (S n)  = m == n
    _     == _      = False

instance Ord Nat where
    compare Z     Z     = EQ
    compare Z     (S _) = LT
    compare (S _) Z     = GT
    compare (S m) (S n) = compare m n

    Z   <= _    = True
    S _ <= Z    =  False
    S m <= S n  = n <= m

    n < m   = not (m <= n)
    n > m   = not (n <= m)
    n >= m  = m <= n

    min Z     _      = Z
    min (S _) Z      = Z
    min (S m) (S n)  = S (min n m)

    max Z       Z       = Z
    max Z       n@(S _) =  n
    max m@(S _) Z       = m
    max (S m)   (S n)   = S (max n m)


instance Num Nat where

    Z   + n  = n
    S m + n  = S (n + m)

    Z   * _  = Z
    S m * n  = (n * m) + m

    abs = id

    signum Z = Z
    signum (S _) = S Z

    negate _ = error "negate @Nat"

    fromInteger n | n < 0  = error "fromInteger @Nat negative"
                  | otherwise = go n where
                  go 0 = Z
                  go n = S (go (n-1))

instance Real Nat where
    toRational = toRational . toInteger


instance Integral Nat where
    toInteger = cata 0 succ

    quotRem _ Z = error "Nat / Z"
    quotRem _ _ = error "quoteRem @Nat not implemented"


instance Enum Nat where
    toEnum n | n < 0     = error "underflow"
             | otherwise = iterate S Z !! n

    fromEnum = cata 0 succ

    succ = S

    pred Z     = error "pred Z"
    pred (S n) = n

instance Show Nat where
--    showsPrec d = showsPrec d . toNatural
   show = show . toNatural

cata z f = go where
    go Z = z
    go (S n) = f (go n)

toNatural :: Nat -> Natural
toNatural Z      = 0
toNatural (S n)  = succ (toNatural n)

fromNatural :: Natural -> Nat
fromNatural 0 = Z
fromNatural n = S (fromNatural (pred n))

nat0 = Z
nat1 = S nat0
nat2 = S nat1
nat3 = S nat2
nat4 = S nat3
nat5 = S nat4
nat6 = S nat5
nat7 = S nat6
nat8 = S nat7
nat9 = S nat8
