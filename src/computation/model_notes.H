#ifndef MODEL_NOTES_H
#define MODEL_NOTES_H

#include <vector>
#include "expression.H"
#include "bounds.H"

// Issue: how to handle unnamed parameters?
// (a) for example, if we have a list (or array?) of parameters that changes.
// (b) how do can we express a computation to look up a parameter?  Such as,
//     "the first parameter in the list"?

class Module;

class Model_Notes
{
protected:
  std::vector<expression_ref> notes;
  std::vector<Module> modules;

public:
  int n_notes() const {return notes.size();}

  const std::vector<expression_ref>& get_notes() const {return notes;}
  std::vector<expression_ref>& get_notes() {return notes;}

  const expression_ref get_note(int i) const {return notes[i];}
  expression_ref get_note(int i) {return notes[i];}

  virtual int add_note(const expression_ref&);
  void add_notes(const std::vector<expression_ref>&);

  int find_match_notes(const expression_ref& e, std::vector<expression_ref>&,int) const;

  int n_modules() const;
  Module& get_module(int i);
  const Module& get_module(int i) const;
  const std::vector<Module>& get_modules() const;
  void add_module(const Module& m);

  Model_Notes();
  Model_Notes(const std::vector<expression_ref>&);
};

Model_Notes add_prefix(const std::string& prefix, const Model_Notes& M);

Model_Notes prefix_formula(const std::string&,const Model_Notes&);

expression_ref def_parameter(Model_Notes&, const std::string& name);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, const Bounds<double>& b);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref&);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref&, const expression_ref&);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, std::nullptr_t, const expression_ref&);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, std::nullptr_t, const expression_ref&, const expression_ref&);

Model_Notes substitute(const Model_Notes& N, const expression_ref& E1, const expression_ref& E2);
std::set<std::string> find_declared_parameters(const std::vector<expression_ref>&);
std::set<std::string> find_declared_parameters(const Model_Notes&);

#endif
