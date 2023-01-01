{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

data T = MkT (forall a.a->a)

f (MkT g) x = g x

id x = x
              
(MkT g, x) = (MkT id, [])
