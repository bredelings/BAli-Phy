#include "model_notes.H"

int Model_Notes::add_note(const expression_ref& E)
{
  for(int i=0;i<notes.size();i++)
    if (notes[i] == E)
      return i;
    else
      assert(notes[i]->print() != E->print());

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

Model_Notes add_prefix(const std::string& prefix, const Model_Notes& M1)
{
  Model_Notes M2;
  for(int i=0;i<M1.n_notes();i++)
    M2.add_note(add_prefix(prefix, M1.get_note(i)));

  return M2;
}
