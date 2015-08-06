module SubAIndex where
{
import Tree;

builtin get_column_index_list_for_characters 2 "get_column_index_list_for_characters" "SubAIndex";
builtin merge_suba_indices 2 "merge_suba_indices" "SubAIndex";

subA_index_leaf t a     = let {indices = mkArray (2*numBranches t) index;
                                              
                               index b = let {s = sourceNode t b} in
                                         if (is_leaf_node t s) then
                                             get_column_index_list_for_characters a (sourceNode t b)
                                         else
                                             case (edgesBeforeEdge t b) of {[b1,b2] -> merge_suba_indices (indices!b1) (indices!b2)}
                              }
                          in indices;
                                                                  
subA_index_internal t a = let {indices = mkArray (2*numBranches t) index;
                               index b = get_column_index_list_for_characters a (sourceNode t b)}
                          in indices;
}
