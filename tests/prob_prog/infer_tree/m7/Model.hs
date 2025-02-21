module Model where

import Bio.Alignment
import Bio.Alphabet
import IModel
import MCMC
import Probability
import SModel
import SModel.Parsimony
import System.Environment
import Tree
import Tree.Newick

gtr_m7_model codons = do
    let nucs = getNucleotides codons

    -- GTR model parameters
    sym <- sample $ symmetricDirichletOn (letter_pair_names nucs) 1
    pi <- sample $ symmetricDirichletOn (getLetters nucs) 1

    let posSelModel w = gtr' sym pi nucs +> x3 codons +> dNdS w

    -- M7 model parameters
    mu <- sample $ uniform 0 1
    gamma <- sample $ beta 1 10
    let m7Model = posSelModel +> m7 mu gamma 4

    let loggers =
            [ "gtr:sym" %=% sym
            , "gtr:pi" %=% pi
            , "m7:mu" %=% mu
            , "m7:gamma" %=% gamma
            ]

    return (m7Model, loggers)

model sequenceData = do
    let taxa = getTaxa sequenceData

    tree <- sample $ uniformLabelledTree taxa (gamma 0.5 (1 / fromIntegral (length taxa)))
    let tlength = treeLength tree

    sigma <- sample $ logLaplace (-3) 1
    indelRates <- fmap (** sigma) <$> sample (iidMap (getUEdgesSet tree) (logNormal 0 1))
    let indelTree = addBranchRates indelRates tree

    scale <- sample $ gamma 0.5 2
    addMove 2 (scaleGroupsSlice [scale] (branchLengths tree))
    addMove 1 (scaleGroupsMH [scale] (branchLengths tree))

    (m7_model, log_m7_model) <- gtr_m7_model (mkCodons dna (geneticCode "standard"))

    rate <- sample $ logLaplace (-4) 0.707
    meanLength <- sample $ shifted_exponential 10 1
    let imodel = IModel.rs07 rate meanLength tree

    let sequenceLengths = getSequenceLengths sequenceData
    (alignment, propertiesA) <- sampleWithProps (phyloAlignment indelTree imodel scale sequenceLengths)
    properties <- observe sequenceData (phyloCTMC tree alignment m7_model scale)

    let alignment_length = alignmentLength alignment
    let num_indels = totalNumIndels alignment
    let total_length_indels = totalLengthIndels alignment
    let prior_A = ln (probability propertiesA)
    let anc_alignment = toFasta (prop_anc_seqs properties)
    let substs = parsimony tree (unitCostMatrix (mkCodons dna standard_code)) (sequenceData, alignment)

    let loggers =
            ["indelRates:sigma" %=% sigma
            , "S1" %>% log_m7_model
            , "rs07:rate" %=% rate
            , "rs07:mean_length" %=% meanLength
            , "scale" %=% scale
            , "scale*|T|" %=% (scale * tlength)
            , "|A|" %=% alignment_length
            , "#indels" %=% num_indels
            , "|indels|" %=% total_length_indels
            , "#substs" %=% substs
            , "prior_A" %=% prior_A
            ]

    return loggers

main logDir = do
    [filename] <- getArgs

    sequenceData <- mkUnalignedCharacterData (mkCodons dna standard_code) <$> load_sequences filename

    return $ model sequenceData
