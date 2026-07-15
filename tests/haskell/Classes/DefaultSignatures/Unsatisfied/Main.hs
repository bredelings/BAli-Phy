{-# LANGUAGE DefaultSignatures, NoImplicitPrelude #-}

class Requirement a

class Identity a where
    convert :: a -> a
    default convert :: Requirement a => a -> a
    convert value = value

data Box = Box

instance Identity Box
