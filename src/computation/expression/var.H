#ifndef DUMMY_H
#define DUMMY_H

#include <set>
#include <string>
#include <optional>
#include <functional>
#include "computation/object.H"
#include "expression_ref.H"
#include "computation/haskell/type.H"

enum class amount_t {Unknown, None, Once, Many};

enum class var_context {unknown, argument};

struct occurrence_info
{
    amount_t work_dup = amount_t::Unknown;

    amount_t code_dup = amount_t::Unknown;

    var_context context = var_context::unknown;

    bool is_loop_breaker = false;

    bool top_level = false;

    bool is_exported = false;

    bool pre_inline() const;
};


// a variable expression
struct var: public Object, public occurrence_info
{
    int index;

    std::string name;

    std::optional<int> level;

    // We use this only to print dvar :: constraint
    std::optional<Haskell::Type> type_;

    bool operator==(const var&) const;
    bool operator==(const Object& o) const;
    var* clone() const {return new var(*this);}
    std::string print() const;
    std::string print_with_type() const;
  
    bool operator<(const var&) const;

    type_constant type() const {return var_type;}

    bool is_wildcard() const {return index < 0 and not name.size();}

    var(int i):index(i) {}
    explicit var(const std::string& s):var(s,0) {}
    var(const std::string& s,int i):index(i),name(s) {assert(name != "_");}
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
