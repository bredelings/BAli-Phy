{-# LANGUAGE NoImplicitPrelude #-}
data Bool = True | False

data C a => D a = Foo (S a)
type S a = [D a]
class C a where
    bar :: a -> D a -> Bool


