module Numeric.Natural (Natural,
                        mkNatural)
    where

newtype Natural = Natural Integer deriving (Eq,Ord)

mkNatural :: Integer -> Natural
mkNatural n | n < 0      = error "mkNatural: negative number!"
            | otherwise  = Natural n

instance Enum Natural where
    toEnum n = Natural (fromIntegral n)
    fromEnum (Natural n) = fromEnum n
    succ (Natural n) = Natural (succ n)
    pred (Natural n) = mkNatural (pred n)

instance Integral Natural where
    toInteger (Natural n) = n

    quotRem _ 0 = error "Natural / 0"
    quotRem _ _ = error "quotRem @Natural unimplemented"

instance Num Natural where
    fromInteger  = mkNatural
    
    Natural m + Natural n = Natural (m+n)
    Natural m * Natural n = Natural (m*n)
    Natural m - Natural n = mkNatural (m-n)

    abs = id

    signum 0 = 0
    signum _ = 1

    negate _ = error "negate @Natural"

instance Real Natural where
    toRational = toRational . toInteger
    
instance Show Natural where
--    showsPrec d (Natural n) = showsPrec d n
  show (Natural n) = show n
