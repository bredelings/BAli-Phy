{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Eq
import Data.Function (($))
import SModel.MutSel (centerFitnesses)
import System.IO (putStrLn)
import Text.Show (show)

-- Check the empty case, the chosen center, preservation of equal groups, and
-- invariance to a common shift in the input fitnesses.
main = do
  putStrLn $ show $ centerFitnesses []
  putStrLn $ show $ centerFitnesses [("AAA", 1), ("AAC", 2), ("AAG", 3)]
  putStrLn $ show $ centerFitnesses [("A", 1), ("B", 1), ("C", 4)]
  putStrLn $ show $
    centerFitnesses [("A", 1), ("B", 2), ("C", 3)] ==
    centerFitnesses [("A", 11), ("B", 12), ("C", 13)]
