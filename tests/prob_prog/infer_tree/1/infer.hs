import Alignment
import Alphabet
import Probability
import Tree
import Data.BitVector
import SModel.Likelihood
import SModel
import SModel.Nucleotides
import SModel.Frequency -- for frequencies_from_dict
import IModel

-- issues: 1. likelihood seems wrong - -1300 vs -700.
--         2. no topology moves included.

main = do
          let alphabet = dna
              a = load_alignment alphabet "5d-muscle.fasta"
              n_tips = (length $ sequences_from_alignment a)
              seqs = sequences_from_alignment a

          -- If n_branches is random, then iid n_branches aborts adding parameters.
          let n_branches = 2*n_tips - 3


          topology <- random $ sample $ uniform_topology n_tips
          ts <- random $ sample $ iid n_branches (gamma 0.5 (2.0/(intToDouble n_branches)))
          scale <- random $ sample $ gamma 0.5 2.0
          let ds = listArray' $ map (*scale) ts

          pi <- random $ sample $ dirichlet_on ["A","C","G","T"] [1.0, 1.0, 1.0, 1.0]
          let pi' = frequencies_from_dict dna pi

          kappa1 <- random $ sample $ log_normal 0.0 1.0
          kappa2 <- random $ sample $ log_normal 0.0 1.0
          let smodel = mmm $ unit_mixture $ tn93 kappa1 kappa2 pi' dna
          
          logLambda <- random $ sample $ log_laplace (-4.0) 0.707
          mean_length <- random $ do l <- sample $ exponential 10.0
                                     return (l+1.0)
          let imodel = rs07 logLambda mean_length topology 1.0 False
              hmms = branch_hmms imodel ds n_branches
              tip_seq_lengths = get_sequence_lengths $ listArray' seqs
          alignment_on_tree <- random $ sample $ random_alignment topology hmms imodel tip_seq_lengths True
          let as = Alignment.pairwise_alignments alignment_on_tree

          let root = targetNode topology 0
              branch_cats = replicate n_branches 0

          observe (ctmc_on_tree topology root as alphabet smodel ts scale branch_cats) seqs

          return $ log_all [write_newick topology %% "topology",
                            ts %% "T",
                            scale %% "scale",
                            kappa1 %% "kappa1",
                            kappa2 %% "kappa2",
                            pi %% "pi"]
