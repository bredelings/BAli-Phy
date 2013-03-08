module Alignment where
{
  import Tree;
  builtin pairwise_alignment_length1 1 "pairwise_alignment_length1";
  builtin pairwise_alignment_length2 1 "pairwise_alignment_length2";
  builtin transition_counts 1 "transition_counts";
  builtin pairwise_alignment_probability_from_counts 2 "pairwise_alignment_probability_from_counts";
  
  branch_hmm model distances heat training b = fst model distances b heat training;
  
  branch_hmms model distances heat training n_branches = listArray' $ map (branch_hmm model distances heat training) [0..n_branches-1];
  
  alignment_branch_pr a hmms b = pairwise_alignment_probability_from_counts (transition_counts (a!b)) (hmms!b);
  
  seqlength a t n = pairwise_alignment_length1 (a!b) where { b = head $ edgesOutOfNode t n};
  
  product' = foldl' (*) (doubleToLogDouble 1.0);
  alignment_pr_top a tree hmm = product' $ map (alignment_branch_pr a hmm) [0..numBranches tree - 1];
  alignment_pr_bot a tree lengthp = (product' $ map (lengthp . seqlength a tree) (internal_nodes tree))^2;
  alignment_pr a tree hmm lengthp = (alignment_pr_top a tree hmm)/(alignment_pr_bot a tree lengthp);
}  