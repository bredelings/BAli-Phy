{-# LANGUAGE DefaultSignatures, NoImplicitPrelude #-}

class Identity a where
    default convert :: a -> a
    convert value = value
