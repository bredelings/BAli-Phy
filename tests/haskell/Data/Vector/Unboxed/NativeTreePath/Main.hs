{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Base (seq)
import Compiler.Num
import Control.Monad (return)
import Data.Foldable (sum)
import Data.Function (($))
import qualified Data.Vector.Unboxed as U
import MCMC
import MCMC.Moves.Tree (walkTreePathVector)
import Probability.Distribution.Tree.UniformTopology (uniformTopology)
import Probability.Random (Random(RanSamplingRate), addMove, makeMCMCModel,
                           sample)
import System.IO (print)

-- Force every native tree-path element before reporting its stable branch
-- count, thereby exercising pair extraction, wrapping, and typed indexing.
pathKernel tree = TransitionKernel $ \context ->
    let path = walkTreePathVector tree context
        total = sum (U.toList path)
    in total `seq` print (U.length path)

-- Suppress the topology distribution's default move and install the path
-- kernel as the only positive-rate transition.
model = do
    tree <- RanSamplingRate 0 $ sample (uniformTopology 5)
    addMove 1 (pathKernel tree)
    return []

-- Construct a real modifiable-tree context and run the sole path kernel once.
main = do
    context <- makeMCMCModel model
    runMCMC 1 context
