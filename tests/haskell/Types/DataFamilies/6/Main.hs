{-# LANGUAGE NoImplicitPrelude #-}

data family F a

data instance F a where
    MkF :: a -> F a

mk :: Int -> F Int
mk x = MkF x

un :: F Int -> Int
un (MkF x) = x
