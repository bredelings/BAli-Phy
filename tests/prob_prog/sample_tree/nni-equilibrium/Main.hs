import Control.Monad (replicateM)
import MCMC
import Probability
import Tree

-- Tip 0 is outside both cherries on 3 of 15 trees. Weighting them by 9/8 gives mass 9/41.
-- A smoke test can miss a biased sweep even when each branch is updated exactly once.
main = do
    context <- makeMCMCState $ do
        tree <- sample $ uniformTopology 5
        let adjacent = head (neighbors tree 0)
            middle = all (\n -> n == 0 || isInternalNode tree n) (neighbors tree adjacent)
        observe 0 $ normal 0 (if middle then 1 else 1.125)
        return []
    samples <- replicateM 2000 $ do
        runMCMC 1 context
        runContextAction logLikelihood context
    -- Only the unit-scale Normal has log density above -1 at zero.
    let count = length (filter (> -1) samples)
    print (350 < count && count < 530)
