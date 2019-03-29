import Alignment
import Alphabet
import Probability
import Tree
import Data.BitVector
import SModel.Likelihood
import SModel
import SModel.Nucleotides
import SModel.Frequency -- for frequencies_from_dict

-- issues: 1. likelihood seems wrong - -1300 vs -700.
--         2. no topology moves included.

main = Strict $ do
          let alphabet = dna
              a = load_alignment alphabet "5d-muscle.fasta"
              n_tips = (length $ sequences_from_alignment a)

          -- If n_branches is random, then iid n_branches aborts adding parameters.
          let n_branches = 2*n_tips - 3


          topology <- Lazy $ sample $ uniform_topology n_tips
          ts <- Lazy $ sample $ iid n_branches (gamma 0.5 (2.0/(intToDouble n_branches)))
          scale <- Lazy $ sample $ gamma 0.5 2.0

          pi <- Lazy $ sample $ dirichlet_on ["A","C","G","T"] [1.0, 1.0, 1.0, 1.0]
          let pi' = frequencies_from_dict dna pi

          kappa1 <- Lazy $ sample $ log_normal 0.0 1.0
          kappa2 <- Lazy $ sample $ log_normal 0.0 1.0
          let smodel = mmm $ unit_mixture $ tn93 kappa1 kappa2 pi' dna
          
          let root = targetNode topology 0
              as = pairwise_alignments_from_matrix a topology
              seqs = sequences_from_alignment a
              branch_cats = replicate n_branches 0

          observe (ctmc_on_tree topology root as alphabet smodel ts scale branch_cats) seqs

          return $ log_all [write_newick topology %% "topology",
                            ts %% "T",
                            scale %% "scale",
                            kappa1 %% "kappa1",
                            kappa2 %% "kappa2",
                            pi %% "pi"]
