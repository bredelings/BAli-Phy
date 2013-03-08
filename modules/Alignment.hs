module Alignment where
{
  import Tree;
  builtin pairwise_alignment_length1 1 "pairwise_alignment_length1";
  builtin pairwise_alignment_length2 1 "pairwise_alignment_length2";
  builtin transition_counts 1 "transition_counts";
  builtin pairwise_alignment_probability_from_counts 2 "pairwise_alignment_probability_from_counts";
  
  alignment_branch_pr a hmm = pairwise_alignment_probability_from_counts (transition_counts a) hmm;
  
  seqlength a t n = pairwise_alignment_length1 (a!b) where { b = head $ edgesOutOfNode t n};
  
  product' = foldl' (*) (doubleToLogDouble 1.0);
  alignment_pr_top a t hmm = product' $ map (alignment_branch_pr a hmm) [0..numNodes t];
  alignment_pr_bot a t lengthp = (product' $ map (lengthp . seqlength a t) (internal_nodes t))^2;
  alignment_pr a t hmm lengthp = (alignment_pr_top a t hmm)/(alignment_pr_bot a t lengthp);
}  