#ifndef DUMMY_H
#define DUMMY_H

#include <set>
#include <string>
#include <optional>
#include <functional>
#include "computation/object.H"
#include "computation/optimization/occurrence_info.H"
#include "expression_ref.H"

struct VarInfo;
struct Type;

// a variable expression
struct var: public Object, public occurrence_info
{
    int index;

    std::string name;

    std::optional<int> level;

    // This stores unfoldings, pragmas, etc.
    std::weak_ptr<const VarInfo> info;

    bool operator==(const var&) const;
    bool operator==(const Object& o) const;
    var* clone() const {return new var(*this);}
    std::string print() const;
  
    bool operator<(const var&) const;

    type_constant type() const {return var_type;}

    bool is_wildcard() const {return index < 0 and not name.size();}

    template <class Archive>
    void serialize(Archive& ar)
    {
	// We should only be serializing var after level / type_ / info are no longer needed.
	ar(name,index);
    }

    var(int i):index(i) {}
    explicit var(const std::string& s):var(s,0) {}
    var(const std::string& s,int i):index(i),name(s) {assert(name != "_");}

    var():index(0) {}
};

expression_ref wildcard();

var qualified_var(const std::string&);

bool is_var(const expression_ref&);

bool is_qualified_var(const expression_ref&);

bool is_wildcard(const expression_ref&);

bool is_wildcard(const var& d);

int max_index(const std::set<var>& s);

std::set<var> get_free_indices(const expression_ref& E);

std::set<var> get_bound_indices(const expression_ref& E);

namespace std
{
    template <>
    class hash < var >{
    public :
        size_t operator()(const var &x) const
        {
            size_t h =   std::hash<int>()(x.index) ^ std::hash<std::string>()(x.name);
            return  h ;
        }
    };
}

#endif
