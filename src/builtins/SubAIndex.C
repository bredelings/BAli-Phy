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

extern "C" closure builtin_function_merge_suba_indices(OperationArgs& Args)
{
  auto i1 = Args.evaluate(0);
  auto i2 = Args.evaluate(1);

  total_subA_index_branch++;
  
  // get the sorted list of present columns
  const Vector<pair<int,int>>& I1 = i1.as_<Vector<pair<int,int>>>();
  const Vector<pair<int,int>>& I2 = i2.as_<Vector<pair<int,int>>>();
  Vector<pair<int,int>> index = combine_columns(I1,I2);

  int l=0;
  for(const auto& I: {I1,I2})
  {
    vector<int> index_to_present_columns = indices_to_present_columns(I, index);
    
    for(int k : index_to_present_columns)
      if (index[k].second == -1)
	index[k].second = l++;
  }
  assert(l == index.size());

  return index;
}
