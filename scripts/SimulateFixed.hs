import Probability.Distribution.PairwiseAlignment
import Bio.Alignment
import Bio.Alphabet
import Effect
import IModel
import MCMC
import Probability
import SModel
import Tree
import Tree.Newick
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.JSON as J
import Probability.Logger
import qualified Data.Text.IO as T
import Data.Array
import System.Environment
import System.IO

-- Read the tree from a file
getTree args = do
  let (filename:_) = args

  rtree <- dropInternalLabels <$> readBranchLengthTree filename

  return rtree

getStartLength args = read startLength' :: Int
    where (_:startLength':_) = args

-- Sample substitution model parameters and define the substitution model
getSmodel = do
  kappaPur <- sample $ logNormal (log 2) 0.25

  kappaPyr <- sample $ logNormal (log 2) 0.25

  pi <- sample $ symmetric_dirichlet_on (letters dna) 2

  return $ tn93' dna kappaPur kappaPyr pi

model rootedTree startLength = do

  smodel <- getSmodel

  -- Sample ancestral sequence STATES
  sequences <- sample $ phyloCTMC rootedTree startLength smodel 1

  -- Return the AlignedCharacterData
  return sequences


main = do
  -- 1. Read the tree and get the starting sequence length
  args <- getArgs

  rootedTree <- getTree args

  let startLength = getStartLength args

  alignedSequences <- run_lazy $ model rootedTree startLength

  T.putStr $ toFasta $ alignedSequences


  
