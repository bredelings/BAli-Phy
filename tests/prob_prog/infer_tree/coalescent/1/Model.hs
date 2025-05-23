module Model where

import           Probability
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           System.Environment  -- for getArgs

smodel_prior nucleotides =  do
    freqs  <- sample $ symmetricDirichletOn (getLetters nucleotides) 1
    kappa1 <- sample $ logNormal (log 2) (1/4)
    kappa2 <- sample $ logNormal (log 2) (1/4)

    let tn93_model = tn93' nucleotides kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)


tree_prior taxa = do

    theta <- sample $ logLaplace (-5) 2

    let taxonAges = [(taxon, 0.0) | taxon <- taxa]
        rateShifts = [(0.0, theta)]

    tree <- sample (coalescentTree taxonAges rateShifts)

    let loggers   = ["tree" %=% writeNewick tree, "theta" %=% theta]
    return (tree, loggers)


model seqData = do
    let taxa = getTaxa seqData

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior dna

    let loggers = tree_loggers ++ ["tn93" %>% sloggers]

    observe seqData $ phyloCTMC tree (alignmentLength seqData) smodel 1

    return loggers

main logDir = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> loadSequences filename

    return $ model seqData
