{-# LANGUAGE DefaultSignatures, NoImplicitPrelude #-}
module DefaultClass where

class Identity a where
    convert :: a -> a
    default convert :: a -> a
    convert value = value
