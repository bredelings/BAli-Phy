import Probability.Distribution.PairwiseAlignment
import Bio.Alignment
import Bio.Alphabet
import Effect
import IModel
import MCMC
import Probability
import Probability.Distribution.OnTree
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
  kappaPur <- sample $ log_normal (log 2) 0.25

  kappaPyr <- sample $ log_normal (log 2) 0.25

  pi <- sample $ symmetric_dirichlet_on (letters dna) 2

  return $ tn93' dna kappaPur kappaPyr pi

-- Get the indel model
getImodel = do
  let mu = 0.05
      lambda = mu
      meanLength = 5

  return $ LongIndels mu lambda meanLength

model rootedTree startLength = do

  smodel <- getSmodel

  imodel <- getImodel

  -- Sample the sequences and their alignment
  alignment <- sample $ IndelsOnTree rootedTree imodel startLength

  -- Sample ancestral sequence STATES
  sequences <- sample $ ctmc_on_tree rootedTree alignment smodel

  -- Return the AlignedCharacterData
  return $ align alignment sequences


main = do
  -- 1. Read the tree and get the starting sequence length
  args <- getArgs

  rootedTree <- getTree args

  let startLength = getStartLength args

  alignedSequences <- run_lazy $ model rootedTree startLength

  T.putStr $ toFasta $ alignedSequences


  
