{-# LANGUAGE NoImplicitPrelude #-}
module Data.Bool where

data Bool = True | False

infixr 3 &&
False && _  = False
_     && x  = x

infixr 2 ||
True  || _  = True
_     || x  = x

not True    =  False
not _       =  True

otherwise = True

bool x y p = if p then x else y
