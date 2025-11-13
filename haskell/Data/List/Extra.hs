{-# LANGUAGE NoImplicitPrelude #-}
module Data.List.Extra where

import Data.List
import Data.Function
import Data.Ord
import Data.Char
import Data.Tuple.Extra
import Compiler.Error (error)    

repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly f [] = []
repeatedly f as = b : repeatedly f as'
    where (b, as') = f as

for :: [a] -> (a -> b) -> [b]
for = flip map

breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn needle haystack | needle `isPrefixOf` haystack = ([], haystack)
breakOn needle [] = ([], [])
breakOn needle (x:xs) = first (x:) $ breakOn needle xs

breakOnEnd :: Eq a => [a] -> [a] -> ([a], [a])
breakOnEnd needle haystack = (reverse *** reverse) $ swap $ breakOn (reverse needle) (reverse haystack)
   
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd f = swap . both reverse . break f . reverse

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd f = breakEnd (not . f)


splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn [] _ = error "splitOn, needle may not be empty"
splitOn _ [] = [[]]
splitOn needle haystack = a : if null b then [] else splitOn needle $ drop (length needle) b
    where (a,b) = breakOn needle haystack

split :: (a -> Bool) -> [a] -> [[a]]
split f [] = [[]]
split f (x:xs) | f x = [] : split f xs
split f (x:xs) | y:ys <- split f xs = (x:y) : ys
