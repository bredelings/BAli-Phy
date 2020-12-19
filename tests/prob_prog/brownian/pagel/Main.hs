import Data.Array           -- for arrays
import System.Environment   -- for getArgs
import Tree                 -- for Tree

-- TODO
-- * implement Tree.Newick.parse
--   - String -> (Tree, [String])  -- node strings
--   - String -> (Tree, [String], [Double]) -- node names and branch lengths
--   - String -> (Tree, [String], [Double], [(String,String)], [(String,String)]) -> node names, branch lengths, node attributes, branch attributes
-- * implement brownian motion likelihood
--   - get matrix of tip-tip distances?
--   - factor in sigma
--   - associate tips and observation with taxon names?
-- * implement readFile
-- * move Tree.write_newick in Tree.Newick
--   -
-- * replace read_file_as_double with some kind of read_csv function, followed by converting the strings to double
-- * Make a TreeWithBranchLengths object.
--   - ideally this implements Tree, but ALSO has functions related to branch lengths.
--   - that requires the full type system though :-S

-- TODO: standardize the haskell
-- * change mkArray n f -> mkarray (n1,n2) f
-- * make getArgs an IO action

-- Question: can we sample a distribution on tips, that we can then observe from, a la prob_prog/sample_tree/3/sample.hs?

treefile =  getArgs !! 0
xs_file =  getArgs !! 0

node_times_from_branch_lengths topology lengths root_time = let node_times = listArray [ get_node_time node | node <- nodes topology ]
                                                                get_node_time node = case (parentBranch topology node) of
                                                                                       None -> root_time
                                                                                       Just b = node_times!parent + lengths!b
                                                            in nodes

scale_internal_node_times topology node_times lambda = mkArray (numNodes topology) (\node -> let time = node_times!node
                                                                                             in if is_leaf_node topology node
                                                                                                then time
                                                                                                else time * lambda)

-- H0: lambda=1, H1: lambda~Uniform[0,1]
sample_lambda_from_prior = do

  h <- bernoulli 0.5

  lambda' <- uniform 0.0 1.0

  let lambda = if h==0 then 1.0 else lambda'

  return (h,lambda)


model topology lengths = do

  (h,lambda) <- sample_lambda_from_prior

  let times = node_times_from_branch_lengths topology lengths 0.0

      times2 = scale_internal_node_times topology times

      lengths2 = branch_lengths_from_node_times

  return (lengths,["lambda" %=% lambda, "H" %=% h])
  

get_tree tree_file = do

  newick_string <- readFile treefile

  return $ Newick.parse newick_string
  

main = do
  (topology,lengths) <- liftIO $ get_tree tree_file

  let xs = read_file_as_double xs_file

  (lengths2, loggers) <- sample $ model topology lengths

  xs ~> phylo_brownian topology lengths2 sigma

  return loggers
