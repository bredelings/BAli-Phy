#ifndef MODEL_NOTES_H
#define MODEL_NOTES_H

#include <vector>
#include "expression.H"

// Issue: how to handle unnamed parameters?
// (a) for example, if we have a list (or array?) of parameters that changes.
// (b) how do can we express a computation to look up a parameter?  Such as,
//     "the first parameter in the list"?

class Model_Notes
{
protected:
  std::vector<expression_ref> notes;

public:
  int n_notes() const {return notes.size();}

  const std::vector<expression_ref>& get_notes() const {return notes;}
  std::vector<expression_ref>& get_notes() {return notes;}

  const expression_ref get_note(int i) const {return notes[i];}
  expression_ref get_note(int i) {return notes[i];}

  virtual int add_note(const expression_ref&);
  void add_notes(const std::vector<expression_ref>&);

  int find_match_notes(const expression_ref& e, std::vector<expression_ref>&,int) const;

  Model_Notes();
  Model_Notes(const std::vector<expression_ref>&);
};

Model_Notes add_prefix(const std::string& prefix, const Model_Notes& M);

#endif
