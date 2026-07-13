{-# LANGUAGE NoImplicitPrelude #-}

type family Choose a where
    Choose Int = Double
    Choose a = Char

data Proxy a = Proxy

bad :: Proxy a -> Choose a -> Char
bad _ x = x
