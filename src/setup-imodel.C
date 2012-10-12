#include "setup.H"
#include "imodel.H"
using std::string;

// FIXME - change to return a (model,standardized name) pair.

/// Return an owned_ptr (possibly NULL) to an IndelModel or type \a name.
owned_ptr<IndelModel> get_imodel(string name) 
{
  //-------------Choose an indel model--------------//
  owned_ptr<IndelModel> imodel;

  // Default
  if (name == "") 
    throw myexception()<<"Indel model name is empty! (A default should have been automatically set.)";

  if (name == "none")
    { }
  else if (name == "RS05")
    imodel = SimpleIndelModel();
  else if (name == "RS07-no-T")
    imodel = NewIndelModel(false);
  else if (name == "RS07")
    imodel = NewIndelModel(true);
  else
    throw myexception()<<"Unrecognized indel model '"<<name<<"'";

  return imodel;
}

