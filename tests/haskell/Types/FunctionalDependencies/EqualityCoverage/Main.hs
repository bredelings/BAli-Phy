{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies, UndecidableInstances #-}

class C a b | a -> b

instance (a ~ b) => C [a] [b]
