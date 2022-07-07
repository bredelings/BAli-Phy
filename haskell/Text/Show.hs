{-# LANGUAGE NoImplicitPrelude #-}
module Text.Show where

import Foreign.String
import Data.List
import Data.Function

foreign import bpcall "Prelude:" show_int :: Int -> CPPString
foreign import bpcall "Prelude:" show_double :: Double -> CPPString

class Show a where
    show :: a -> [Char]
    showList :: [a] -> [Char]

    showList [] = "[]"
    showList (x:y) = "["++show x++show' y++"]" where show' [] = ""
                                                     show' (x:y) = ","++show x++show' y

instance Show Char where
    show  c = ['\'',c,'\'']
    showList s = "\"" ++ s ++ "\""

instance Show Int where
    show i = unpack_cpp_string $ show_int i

instance Show Double where
    show  d = unpack_cpp_string $ show_double d

instance Show () where
    show _ = "()"

instance (Show a, Show b) => Show (a,b) where
    show (x,y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance (Show a, Show b, Show c) => Show (a,b,c) where
    show (x,y,z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance (Show a, Show b, Show c, Show d) => Show (a,b,c,d) where
    show (x,y,z,w) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show w ++ ")"

instance Show a => Show [a] where
    show s = showList s

