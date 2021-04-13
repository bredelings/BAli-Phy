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

    let mut_sel_model w = gtr' sym pi nucleotides & SModel.x3 codons & dNdS w & mut_sel' ws
        m3_model = mut_sel_model & SModel.m3 ps omegas

    let loggers = ["gtr:sym" %=% sym, "gtr:pi" %=% pi,
                   "mut_sel:2ns" %=% ws,
                   "m3:ps" %=% ps, "m3:omegas" %=% omegas]

    return (m3_model, loggers)

tree_prior taxa = do

    topology <- uniform_labelled_topology taxa
    let b = numBranches topology
    times <- iid b (gamma 0.5 (2.0 / intToDouble b))
    scale <- gamma 0.5 2.0
    let tree      = scale_branch_lengths scale $ branch_length_tree topology times

    let loggers   = ["tree" %=% write_newick tree, "scale" %=% scale]
    return (tree, loggers)


prior taxa = do

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior (codons dna standard_code)

    let loggers = tree_loggers ++ ["S1" %>% sloggers]

    return (tree, smodel, loggers)


observe_data seq_data = do
    let taxa = map sequence_name seq_data

    (tree, smodel, loggers) <- sample $ prior taxa

    seq_data ~> ctmc_on_tree_fixed_A tree smodel

    return loggers

main = do
    args <- getArgs

    let filename = args !! 0
        seq_data = load_sequences filename

    mcmc $ observe_data seq_data
