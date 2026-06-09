{-# LANGUAGE NoImplicitPrelude #-}

data family F a

data instance F Int = MkF { getF :: Int }

mk :: Int -> F Int
mk x = MkF x

un :: F Int -> Int
un x = getF x
