module Model where

import           Probability
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           System.Environment  -- for getArgs

-- Non-zero branches are slightly longer to keep to average length correct.
-- The division means that we can't set the value though.
maybe_zero p dist = do
    is_zero <- prior $ bernoulli p
    length  <- prior $ dist
    if is_zero == 1 then return 0 else return (length / (1 - p))

branchLengthDist zero_p topology b | isInternalBranch topology b = branch_dist_internal
                                   | otherwise                   = branch_dist_leaf
  where
    n                    = numBranches topology
    branch_dist_leaf     = prior $ gamma 0.5 (2 / fromIntegral n)
    branch_dist_internal = maybe_zero zero_p branch_dist_leaf

model seqData = do

    let taxa = getTaxa seqData

    zeroP <- prior $ beta 0.1 1

    scale  <- prior $ gamma 0.5 2

    tree   <- prior $ uniformLabelledTree'' taxa (branchLengthDist zeroP)

    freqs  <- prior $ symmetricDirichletOn (getLetters dna) 1

    kappa1 <- prior $ logNormal 0 1

    kappa2 <- prior $ logNormal 0 1

    let tn93_model = tn93' dna kappa1 kappa2 freqs

    observe seqData $ phyloCTMC tree (alignmentLength seqData) tn93_model scale

    return
        [ "tree" %=% writeNewick tree
        , "scale" %=% scale
        , "zeroP" %=% zeroP
        , "kappa1" %=% kappa1
        , "kappa2" %=% kappa2
        , "frequencies" %=% freqs
        ]

main logDir = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> load_sequences filename

    return $ model seqData
