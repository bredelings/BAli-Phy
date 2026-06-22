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
    std::string name;

    bool operator==(const var&) const;
    bool operator==(const Object& o) const;
    var* clone() const {return new var(*this);}
    std::string print() const;
  
    bool operator<(const var&) const;

    type_constant type() const {return type_constant::var_type;}

    template <class Archive>
    void serialize(Archive& ar)
    {
	// We should only be serializing var after level is no longer needed.
	ar(name);
    }

    explicit var(const std::string& s):name(s) {}
};

bool is_var(const expression_ref&);

std::set<var> get_free_indices(const expression_ref& E);

#endif
