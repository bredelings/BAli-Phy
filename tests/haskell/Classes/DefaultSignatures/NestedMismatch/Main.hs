{-# LANGUAGE DefaultSignatures, NoImplicitPrelude, RankNTypes #-}

class Nested a where
    method :: a -> forall b c. b -> c -> a
    default method :: a -> forall c b. b -> c -> a
    method value _ _ = value
