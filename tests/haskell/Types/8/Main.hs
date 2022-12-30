{-# LANGUAGE NoImplicitPrelude #-}

data T = T

f :: T -> T
f x = x


g x = f [x]
