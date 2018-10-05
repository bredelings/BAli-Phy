{-# LANGUAGE NoImplicitPrelude #-}
module Data.Bool where

data Bool = True | False

infixr 3 &&
True  && x = x
False && x = False

infixr 2 ||
True  || x  = True
False || x = x

not True         =  False
not False        =  True

otherwise = True

