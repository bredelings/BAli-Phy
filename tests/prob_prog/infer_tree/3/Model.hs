module Model where

import           Probability
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           System.Environment  -- for getArgs

smodel_prior codons = do
    let nucleotides = getNucleotides codons
    sym <- prior $ symmetricDirichletOn (letter_pair_names nucleotides) 1.0
    pi  <- prior $ symmetricDirichletOn (letters nucleotides) 1.0
    ws   <- zip (letters codons) <$> prior (iid (length (letters codons)) (normal 0 1))
    let n  = 4
    omegaDist <- prior $ dirichletMixture n (uniform 0 1) 2

    let mut_sel_model w = gtr' sym pi nucleotides +> SModel.x3 codons +> dNdS w +> mut_sel' ws
        m3_model = mut_sel_model +> SModel.m3 omegaDist

    let loggers = ["gtr:sym" %=% sym,
                   "gtr:pi" %=% pi,
                   "mut_sel:2ns" %=% ws,
                   "m3:omegaDist" %=% sortDist omegaDist]

    return (m3_model, loggers)

branch_length_dist topology b = gamma 0.5 (2.0 / fromIntegral n) where n = numBranches topology

model seqData = do

    let taxa = getTaxa seqData

    scale <- prior $ gamma 0.5 2.0

    tree <- prior $ uniform_labelled_tree taxa branch_length_dist

    (smodel, sloggers    ) <- smodel_prior (mkCodons dna standard_code)

    let loggers = ["tree" %=% write_newick tree, "scale" %=% scale, "S1" %>% sloggers]

    observe seqData $ phyloCTMC tree (alignmentLength seqData) smodel scale

    return loggers

main = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> load_sequences filename

    return $ model seqData
