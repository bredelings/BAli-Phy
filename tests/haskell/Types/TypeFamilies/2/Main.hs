{-# LANGUAGE NoImplicitPrelude #-}

import Data.Bool
import Data.Eq

class Collects ce where
    type Elem ce
    empty :: ce
    insert :: Elem ce -> ce -> ce
    member :: Elem ce -> ce -> Bool
    toList :: ce -> [Elem ce]


instance Eq e => Collects [e] where
    type Elem [e]   = e
    empty           = []
    insert e l      = (e:l)
    member e []     = False
    member e (x:xs)
        | e == x    = True
        | otherwise = member e xs
    toList l        = l

foldr f z [] = z
foldr f z (x:xs) = (f x (foldr f z xs))

sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2
sumCollects c1 c2 = foldr insert c2 (toList c1)               
