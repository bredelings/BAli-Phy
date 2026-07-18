{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies, UndecidableInstances #-}

data A = A
data D = D
data R = R
data Pair a b = Pair a b
data L a = L a
data M a = M a

class Dep c x | c -> x where
    extract :: c -> x

class C a b c | c -> a b where
    convert :: c -> Pair a b

instance Dep c x => C (L x) (M x) c where
    convert x = Pair (L (extract x)) (M (extract x))

class Q a where
    consume :: a -> R

instance Q (Pair (L A) (M A)) where
    consume _ = R

improvesBothResults :: Dep D A => R
improvesBothResults = consume (convert D)
