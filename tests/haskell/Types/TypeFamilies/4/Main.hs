{-# LANGUAGE NoImplicitPrelude #-}

type family Arg a
type family Result a

type instance Arg (c -> d) = c
type instance Result (c -> d) = d

class C a where
    method :: a -> a

instance C (a -> b) where
    method x = x

foo :: (a ~ (Arg a -> Result a)) => a -> a
foo x = x

func x = method (foo x)
