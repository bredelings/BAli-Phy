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
