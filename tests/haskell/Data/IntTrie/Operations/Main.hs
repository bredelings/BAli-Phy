{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Data.IntTrie
import Data.Bits (bit)
import Data.Function (($))
import Data.Functor (fmap)
import Data.Semigroup ((<>))
import Data.Monoid (mempty)
import Data.List (map, reverse)
import Compiler.Num
import Control.Applicative
import System.IO (print)

-- Check signed and large keys, updates, and productive infinite inputs. Ordinary finite-map tests
-- cannot protect these lazy-trie guarantees; retain while the vendored implementation is used.
main = do
 let t = identity :: IntTrie Integer
     keys = [-7,-2,-1,0,1,2,7] :: [Integer]
     changes = [(-2,(+10)),(0,(+20)),(2,(+30))] :: [(Integer,Integer -> Integer)]
 print (map (apply t) keys)
 print (apply t (bit 100::Integer), apply t (negate (bit 100)::Integer))
 print (apply (identity::IntTrie Int) (-123::Int))
 print (map (apply (modify (-2::Integer) (+10) t)) keys)
 print (map (apply (modify' (0::Integer) (+10) t)) keys)
 print (map (apply (overwrite (2::Integer) 99 t)) keys)
 print (map (apply (mirror t)) keys)
 print (map (apply (modifyAscList changes t)) keys)
 print (map (apply (modifyDescList (reverse changes) t)) keys)
 let evens = modifyAscList [(i,(+100)) | i <- ([2,4..]::[Integer])] t
 print (map (apply evens) ([1,2,3,4,7,8]::[Integer]))
 print (apply (pure (+) <*> t <*> fmap (*2) t) (3::Integer))
 print (apply (pure [1::Int] <> pure [2] <> mempty) (0::Int))
