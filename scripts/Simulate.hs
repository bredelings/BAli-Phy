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
--  T.putStrLn $ write_newick rtree

  return rtree

getStartLength args = read startLength' :: Int
    where (_:startLength':_) = args

-- Sample substitution model parameters and define the substitution model
getSmodel = do
  kappaPur <- sampleIO $ log_normal (log 2) 0.25

  kappaPyr <- sampleIO $ log_normal (log 2) 0.25

  pi <- sampleIO $ symmetric_dirichlet_on (letters dna) 1

  return $ tn93' dna kappaPur kappaPyr pi

-- Get the indel model
getImodel = do
  let mu = 0.05
      lambda = mu
      meanLength = 5

  return $ LongIndels mu lambda meanLength

main = do
  -- 1. Read the tree and get the starting sequence length
  args <- getArgs

  rtree <- getTree args

  let startLength = getStartLength args

  smodel <- getSmodel

  imodel <- getImodel

  -- 4. Sample the sequences and their alignment
  alignment <- sampleIO $ IndelsOnTree rtree imodel startLength
--  putStrLn $ "lengths = " ++ show (sequence_lengths alignment)
--  putStrLn $ show $ pairwise_alignments alignment

  -- 5. Sample ancestral sequence STATES
  stateSequences <- (extractStates <$>) <$> sampleComponentStates rtree alignment smodel
  let alignedStateSequences = alignedSequences alignment stateSequences
      alignedLetterSequences = fmap (statesToLetters (stateLetters smodel)) alignedStateSequences
      alignedTextSequences = fmap (sequenceToText dna) alignedLetterSequences
  let fasta = fastaSeqs $ getLabelled rtree Sequence alignedTextSequences
  T.putStr fasta

{-
  -- putStrLn $ "componentStateSequences = " ++ show componentStateSequences

  -- 5. Sample aligned sequences
  sequences <- sampleIO $ ctmc_on_tree rtree alignment smodel
  putStrLn "FASTA ="
  sequence [T.putStr (fastaSeq s)|  s <- sequences]
-}
  return ()


  
