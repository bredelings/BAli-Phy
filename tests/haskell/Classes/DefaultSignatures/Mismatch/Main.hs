{-# LANGUAGE DefaultSignatures, NoImplicitPrelude #-}

data Other = Other

class Identity a where
    convert :: a -> a
    default convert :: a -> Other
    convert _ = Other
