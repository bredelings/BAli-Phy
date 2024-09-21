module Model where

import           Probability
import           Bio.Alignment
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           IModel
import           System.Environment  -- for getArgs

branch_length_dist topology branch = gamma (1/2) (2/fromIntegral n) where n = numBranches topology

model seq_data = do
    let taxa            = getTaxa seq_data
        tip_seq_lengths = get_sequence_lengths seq_data

    -- Tree
    scale <- prior $ gamma (1/2) 2
    tree  <- prior $ uniformLabelledTree'' taxa branch_length_dist

    -- Indel model
    indel_rate   <- prior $ logLaplace (-4) 0.707
    mean_length <- (1 +) <$> sample (exponential 10)
    let imodel = rs07 indel_rate mean_length tree

    -- Substitution model
    freqs  <- prior $ symmetricDirichletOn ["A", "C", "G", "T"] 1
    kappa1 <- prior $ logNormal 0 1
    kappa2 <- prior $ logNormal 0 1
    let tn93_model = tn93' dna kappa1 kappa2 freqs

    -- Alignment
    alignment <- prior $ phyloAlignment tree imodel scale tip_seq_lengths

    -- Observation
    observe seq_data $ phyloCTMC tree alignment tn93_model scale

    return
        [ "tree" %=% writeNewick tree
        , "log(indel_rate)" %=% log indel_rate
        , "mean_length" %=% mean_length
        , "kappa1" %=% kappa1
        , "kappa2" %=% kappa2
        , "frequencies" %=% freqs
        , "scale" %=% scale
        , "|T|" %=% treeLength tree
        , "scale*|T|" %=% treeLength tree * scale
        , "|A|" %=% alignmentLength alignment
        ]

main = do
    [filename] <- getArgs

    seq_data <- mkUnalignedCharacterData dna <$> load_sequences filename

    return $ model seq_data
