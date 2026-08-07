import Probability
import Probability.Random
import Probability.Distribution.Tree.Yule
import qualified Data.Text as T
import Tree.Newick
import Data.JSON as J
import Data.Char
import Options.Applicative

model taxa = do
    lambda <- sample $ logLaplace 0 2

    tree <- sample $ yule taxa lambda

    let loggers   = ["tree" %=% writeNewick tree, "lambda" %=% lambda]
    return (tree, loggers)

name i | i < 26 = [chr(65+i)]
       | otherwise = [chr(65+m)] ++ name d
    where (d,m) = divMod i 26

-- Parse the required taxon count before constructing taxon names.
options = info
     (argument auto (metavar "N-TAXA" <> help "Number of taxa") <**> helper)
     (fullDesc <> progDesc "Sample a tree from the Yule model")

main = do
     n <- execParser options
     let taxa = fmap T.pack $ fmap name [0..n-1]

     (tree,loggers) <- runRandomStrict (model taxa)
     putStrLn $ show $ J.Object loggers
