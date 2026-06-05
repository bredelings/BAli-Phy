#ifndef DUMMY_H
#define DUMMY_H

#include <set>
#include <string>
#include <optional>
#include "computation/object.H"
#include "computation/optimization/occurrence_info.H"
#include "expression_ref.H"

// a variable expression
struct var: public Object, public occurrence_info
{
    int index;

    std::string name;

    std::optional<int> level;

    bool is_exported = false;

    bool operator==(const var&) const;
    bool operator==(const Object& o) const;
    var* clone() const {return new var(*this);}
    std::string print() const;
  
    bool operator<(const var&) const;

    type_constant type() const {return type_constant::var_type;}

    bool is_wildcard() const {return index < 0 and not name.size();}

    template <class Archive>
    void serialize(Archive& ar)
    {
	// We should only be serializing var after level is no longer needed.
	ar(name,index,is_exported);
    }

    var(int i):index(i) {}
    explicit var(const std::string& s):var(s,0) {}
    var(const std::string& s,int i,bool e=false):index(i),name(s),is_exported(e) {assert(name != "_");}

    var():index(0) {}
};

bool is_var(const expression_ref&);

std::set<var> get_free_indices(const expression_ref& E);

#endif
