import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

smodel_prior nucs = do
    freqs  <- symmetric_dirichlet_on (letters nucs) 1.0
    kappa1 <- log_normal 0.0 1.0
    kappa2 <- log_normal 0.0 1.0

    let tn93_model = tn93' nucs kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)

-- Non-zero branches are slightly longer to keep to average length correct.
maybe_zero p dist = do
  is_zero <- bernoulli p
  length <- dist
  if is_zero == 1 then
      return 0.0
  else
      return (length/(1.0-p))

tree_prior taxa = do

    topology <- uniform_labelled_topology taxa

    zero_p <- beta 0.1 1.0

    let nb = numBranches topology
        branch_dist_leaf = gamma 0.5 (2.0/ intToDouble nb)
        branch_dist_internal = maybe_zero zero_p branch_dist_leaf
        branch_dist b | is_internal_branch topology b = branch_dist_internal
                      | otherwise                     = branch_dist_leaf

    times <- independent [branch_dist b | b <- [0..nb-1]]

    scale <- gamma 0.5 2.0
    let distances = map (scale *) times

    let tree      = branch_length_tree topology distances

    let loggers   = ["tree" %=% write_newick tree, "scale" %=% scale, "zero_p" %=% zero_p]
    return (tree, loggers)


prior taxa = do

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, smodel_loggers    ) <- smodel_prior dna

    let loggers = tree_loggers ++ ["tn93" %>% smodel_loggers]

    return (tree, smodel, loggers)


model seq_data = do

    let taxa = map sequence_name seq_data

    (tree, smodel, loggers) <- sample $ prior taxa

    seq_data ~> ctmc_on_tree_fixed_A tree smodel

    return loggers

main = do
    args <- getArgs

    let filename = args !! 0
        seq_data = load_sequences filename

    mcmc $ model seq_data
