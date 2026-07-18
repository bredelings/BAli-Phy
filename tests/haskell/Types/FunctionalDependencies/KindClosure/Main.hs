{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies, KindSignatures, PolyKinds #-}

data A = A
data B = B
data Pair a b = Pair a b
data F a = F a

-- NOTE: Source kinds currently support Type and arrows, but not kind variables.
data Proxy (f :: Type -> Type) = Proxy

class C a (f :: Type -> Type) | a -> f where
    convert :: a -> Proxy f

kindClosure x =
    let inner y = Pair (convert x) y
    in Pair (inner A) (inner B)
