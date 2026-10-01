{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Bio.Alignment (observationsOnTree)
import Compiler.Num (fromInteger)
import Data.Functor (fmap)
import qualified Data.IntMap as IntMap
import Data.List (sort, tail)
import Data.Maybe (Maybe)
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.IO (IO, print)
import Tree (getLabel, getNodes)
import Tree.Newick (readTreeTopology)

-- Check exact leaf/internal-node associations and both mismatch directions independently of infer.
-- Demanding only map size must still reject mismatches; ordinary model tests do not ensure this.
-- The error cases become obsolete if partial matching is intentionally allowed.
main :: IO ()
main = do
    tree <- readTreeTopology "../observations.nwk"
    args <- getArgs
    let base = [(Text.pack n, v) | (n,v) <- [("one",1), ("two",2), ("three",3), ("ancestor",4)]]
        observations = case args of
            ["extra"] -> (Text.pack "absent", 5) : base
            ["missing"] -> tail base
            _ -> base
        matched = observationsOnTree tree observations :: IntMap.IntMap (Maybe Int)
    case args of
        ["valid"] -> print (sort [(fmap Text.unpack (getLabel tree node), matched IntMap.! node)
                                 | node <- getNodes tree])
        _ -> print (IntMap.size matched)
