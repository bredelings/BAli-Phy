{-# LANGUAGE NoImplicitPrelude #-}

data A = A
data B = B

class C a where
    data D a

instance C A where
    data D B = MkD B
