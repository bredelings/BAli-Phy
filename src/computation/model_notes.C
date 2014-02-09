#include "model_notes.H"

using std::vector;
using std::set;
using std::string;

int Model_Notes::add_note(const expression_ref& E)
{
  for(int i=0;i<notes.size();i++)
    if (notes[i] == E)
      return i;
    else if (notes[i]->compare(*E))
      return i;
  // This causes trouble if we have unprintable elements in the note:
  //    else
  //      assert(notes[i]->print() != E->print());

  notes.push_back(E);
  return notes.size()-1;
}

void Model_Notes::add_notes(const std::vector<expression_ref>& N)
{
  // Note: It is quite likely that we'll add notes that we already contain.
  //       That will be handled by add_note( ).

  for(int i=0;i<N.size();i++)
    for(int j=0;j<i;j++)
      assert(N[i] != N[j]);

  for(int i=0;i<N.size();i++)
    add_note(N[i]);
}

int Model_Notes::find_match_notes(const expression_ref& query, std::vector<expression_ref>& results, int start) const
{
  assert(start >= 0);
  for(int i=start;i<n_notes();i++)
  {
    results.clear();
    if (find_match(query, get_note(i), results))
      return i;
  }
  return -1;
}

Model_Notes::Model_Notes()
{ }

Model_Notes::Model_Notes(const std::vector<expression_ref>& N)
{
  add_notes(N);
}

Model_Notes substitute(const Model_Notes& N, const expression_ref& E1, const expression_ref& E2)
{
  Model_Notes N2 = N;
  for(auto& n: N2.get_notes())
    n = substitute(n, E1, E2);
  return N2;
}
