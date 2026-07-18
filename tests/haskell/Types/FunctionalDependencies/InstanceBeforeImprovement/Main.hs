{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data B = B

class C a b | a -> b where
    convert :: a -> b

instance C A B where
    convert _ = B

usesInstance :: C A b => A -> B
usesInstance = convert
