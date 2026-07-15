{-# LANGUAGE DefaultSignatures, NoImplicitPrelude, RankNTypes #-}

class Reordered a where
    method :: forall b c. a -> b -> c -> a
    default method :: forall c b. a -> b -> c -> a
    method value _ _ = value

    renamed :: forall b. a -> b -> a
    default renamed :: forall x. a -> x -> a
    renamed value _ = value
