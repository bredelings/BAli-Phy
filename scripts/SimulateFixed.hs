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
import Options.Applicative
import Probability.Logger
import qualified Data.Text.IO as T
import System.IO

-- Read the tree from a file
getTree filename = do
  rtree <- dropInternalLabels <$> readBranchLengthTree filename

  return rtree

-- Require both simulation inputs and provide controlled help and parse errors.
options = info
  ((,) <$> strArgument (metavar "TREE" <> help "Newick tree file")
       <*> argument auto (metavar "START-LENGTH" <> help "Starting sequence length")
       <**> helper)
  (fullDesc <> progDesc "Simulate sequences along a fixed tree")

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
  (treeFile, startLength) <- execParser options

  rootedTree <- getTree treeFile

  alignedSequences <- runRandomLazy $ model rootedTree startLength

  T.putStr $ toFasta $ alignedSequences


  
