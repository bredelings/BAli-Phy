{-# LANGUAGE NoImplicitPrelude #-}

type family Choose a where
    Choose Int = Double
    Choose a = Char

data Proxy a = Proxy

chooseInt :: Choose Int -> Double
chooseInt x = x

chooseDouble :: Choose Double -> Char
chooseDouble x = x

chooseGiven :: a ~ Double => Proxy a -> Choose a -> Char
chooseGiven _ x = x
