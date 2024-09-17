import Probability
import Probability.Random
import Probability.Distribution.Tree.Coalescent
import qualified Data.Text as T
import Tree.Newick
import Data.JSON as J
import Data.Char

model taxa = do
    theta <- sample $ logLaplace (-5) 2.0

    let nTaxa = length taxa
        leafTimes = zip (replicate nTaxa 0) [0..]
        rateShifts = []

    tree <- add_labels taxa <$> sample (coalescentTree theta leafTimes rateShifts)

    let loggers   = ["tree" %=% write_newick tree, "theta" %=% theta]
    return (tree, loggers)

name i | i < 26 = [chr(65+i)]
       | otherwise = [chr(65+m)] ++ name d
    where (d,m) = divMod i 26

main = do
     let n = 20
         taxa = zip [0..n] (fmap T.pack $ fmap name [0..])

     (tree,loggers) <- run_strict (model taxa)
     putStrLn $ show $ J.Object loggers

