#include "note_collection.H"

using std::vector;

note_ptr_t note_collection::add_note(const expression_ref& E)
{
  return notes.insert(E).first;
}

void note_collection::erase_note(const expression_ref& E)
{
  notes.erase(E);
}

void note_collection::erase_note(note_ptr_t n)
{
  notes.erase(n);
}

note_ptr_t note_collection::find_match_note(const expression_ref& query, std::vector<expression_ref>& results, note_ptr_t start) const
{
  for(note_ptr_t i = start; i != notes.end(); i++)
    if (find_match(query, *i, results))
      return i;

  return notes.end();
}

note_ptr_t note_collection::find_match_note(const expression_ref& query, std::vector<expression_ref>& results) const
{
  return find_match_note(query, results, begin());
}

vector<vector<expression_ref> > note_collection::find_match_notes(const expression_ref& query) const
{
  vector<vector<expression_ref> > results;

  bool done = false;
  note_ptr_t loc = begin();
  while (not done)
  {
    vector<expression_ref> R;
    loc = find_match_note(query, R, loc);
    if (loc == end())
      done = true;
    else {
      results.push_back(R);
      loc++;
    }
  }

  return results;
}


