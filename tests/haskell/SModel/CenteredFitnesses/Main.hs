{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Compiler.Base (String)
import Data.Eq
import Data.Function (($))
import qualified Data.Map as Map
import SModel.MutSel (centerFitnesses)
import System.IO (putStrLn)
import Text.Show (show)

fitnessMap :: [(String, Double)] -> Map.Map String Double
fitnessMap = Map.fromList

-- Check the empty case, the chosen center, preservation of equal groups, and
-- invariance to a common shift in the input fitnesses.
main = do
  putStrLn $ show $ Map.toAscList $ centerFitnesses $ fitnessMap []
  putStrLn $ show $ Map.toAscList $ centerFitnesses $ fitnessMap [("AAA", 1), ("AAC", 2), ("AAG", 3)]
  putStrLn $ show $ Map.toAscList $ centerFitnesses $ fitnessMap [("A", 1), ("B", 1), ("C", 4)]
  putStrLn $ show $
    centerFitnesses (fitnessMap [("A", 1), ("B", 2), ("C", 3)]) ==
    centerFitnesses (fitnessMap [("A", 11), ("B", 12), ("C", 13)])
