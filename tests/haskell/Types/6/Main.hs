{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}

data T a = forall b.T (b -> a) b

apply (T f x) = x
