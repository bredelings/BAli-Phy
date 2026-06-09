{-# LANGUAGE NoImplicitPrelude #-}

class C a
class D a

data (C a, D a) => T a = MkT a
