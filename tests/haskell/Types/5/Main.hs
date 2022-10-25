{-# LANGUAGE NoImplicitPrelude #-}

data T a = forall b.T (b -> a) b

apply (T f x) = f x
