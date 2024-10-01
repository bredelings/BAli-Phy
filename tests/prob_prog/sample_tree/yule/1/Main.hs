import Probability
import Probability.Random
import Probability.Distribution.Tree.Yule
import qualified Data.Text as T
import Tree.Newick
import Data.JSON as J
import Data.Char
import System.Environment (getArgs)

model taxa = do
    lambda <- sample $ logLaplace 0 2

    tree <- sample $ yule taxa lambda

    let loggers   = ["tree" %=% writeNewick tree, "lambda" %=% lambda]
    return (tree, loggers)

name i | i < 26 = [chr(65+i)]
       | otherwise = [chr(65+m)] ++ name d
    where (d,m) = divMod i 26

main = do
     [args] <- getArgs
     let n = read args :: Int
         taxa = fmap T.pack $ fmap name [0..n-1]

     (tree,loggers) <- runRandomStrict (model taxa)
     putStrLn $ show $ J.Object loggers
