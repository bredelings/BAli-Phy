import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

smodel_prior codons = do
    let nucleotides = getNucleotides codons
    sym <- symmetric_dirichlet_on (letter_pair_names nucleotides) 1.0
    pi  <- symmetric_dirichlet_on (letters nucleotides) 1.0
    ws   <- zip (letters codons) <$> iid (length (letters codons)) (normal 0.0 1.0)
    let n  = 4
    ps     <- symmetric_dirichlet n 2.0
    omegas <- iid n (uniform 0.0 1.0)

    let mut_sel_model w = gtr' sym pi nucleotides +> SModel.x3 codons +> dNdS w +> mut_sel' ws
        m3_model = mut_sel_model +> SModel.m3 ps omegas

    let loggers = ["gtr:sym" %=% sym,
                   "gtr:pi" %=% pi,
                   "mut_sel:2ns" %=% ws,
                   "m3:ps" %=% ps,
                   "m3:omegas" %=% omegas]

    return (m3_model, loggers)

branch_length_dist topology b = gamma 0.5 (2.0 / fromIntegral n) where n = numBranches topology

model seq_data = do

    let taxa = map sequence_name seq_data

    scale <- gamma 0.5 2.0

    tree <- scale_branch_lengths scale <$> uniform_labelled_tree taxa branch_length_dist

    (smodel, sloggers    ) <- smodel_prior (codons dna standard_code)

    let loggers = ["tree" %=% write_newick (make_rooted tree), "scale" %=% scale, "S1" %>% sloggers]

    seq_data ~> ctmc_on_tree_fixed_A tree smodel

    return loggers

main = do
    [filename] <- getArgs

    seq_data <- load_sequences filename

    mcmc $ model seq_data
