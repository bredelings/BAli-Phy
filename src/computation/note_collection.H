#ifndef NOTE_COLLECTION_H
#define NOTE_COLLECTION_H

#include <set>
#include <vector>
#include "expression.H"

typedef std::set<expression_ref>::iterator note_ptr_t;

class note_collection
{
  std::set<expression_ref> notes;

public:

  note_ptr_t add_note(const expression_ref&);

  void erase_note(const expression_ref& E);

  void erase_note(note_ptr_t);

  const std::set<expression_ref>& get_notes() const {return notes;}

  int n_notes() const {return notes.size();}

  note_ptr_t begin() const {return notes.begin();}

  note_ptr_t end() const {return notes.end();}

  note_ptr_t find_match_note(const expression_ref& e, std::vector<expression_ref>&, note_ptr_t) const;

  note_ptr_t find_match_note(const expression_ref& e, std::vector<expression_ref>&) const;

  std::vector<std::vector<expression_ref> > find_match_notes(const expression_ref& e) const;
};

#endif
