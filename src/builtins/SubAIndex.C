#include "computation/computation.H"
#include "alignment/alignment.C"
#include "substitution/substitution-index.H"
#include <utility>

using std::vector;
using std::pair;

extern int total_subA_index_branch;

extern "C" closure builtin_function_get_column_index_list_for_characters(OperationArgs& Args)
{
  auto a = Args.evaluate(0);
  int n = Args.evaluate(1).as_int();

  return new Vector<pair<int,int>> (convert_to_column_index_list(a.as_<alignment>().get_columns_for_characters(n)));
}

