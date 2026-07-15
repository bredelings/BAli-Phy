{-# LANGUAGE DefaultSignatures, NoImplicitPrelude #-}

class Identity a where
    convert :: a -> a
    default convert :: a -> a
    default convert :: a -> a
    convert value = value
