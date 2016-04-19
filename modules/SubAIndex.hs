module SubAIndex where
{
import Tree;

builtin get_column_index_list_for_characters 2 "get_column_index_list_for_characters" "SubAIndex";

subA_index_internal t a = let {indices = mkArray (2*numBranches t) index;
                               index b = get_column_index_list_for_characters a (sourceNode t b)}
                          in indices;
}
