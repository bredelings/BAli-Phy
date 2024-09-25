import Probability
import Probability.Random
import Probability.Distribution.Tree.Coalescent
import qualified Data.Text as T
import Tree.Newick
import Data.JSON as J
import Data.Char

model taxa = do
    theta <- sample $ logLaplace 2 2

    let nTaxa = length taxa
        taxonAges = zip taxa [0..]
        rateShifts = [(0,theta)]

    tree <- sample $ coalescentTree taxonAges rateShifts

    let loggers   = ["tree" %=% writeNewick tree, "theta" %=% theta]
    return (tree, loggers)

name i | i < 26 = [chr(65+i)]
       | otherwise = [chr(65+m)] ++ name d
    where (d,m) = divMod i 26

main = do
     let n = 20
         taxa = fmap T.pack $ fmap name [0..n-1]

     (tree,loggers) <- runRandomStrict (model taxa)
     putStrLn $ show $ J.Object loggers
