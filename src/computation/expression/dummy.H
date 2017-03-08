#ifndef DUMMY_H
#define DUMMY_H

#include "object.H"
#include "expression_ref.H"

enum class amount_t {Unknown, None, Once, Many};

enum class var_context {unknown, argument};

struct occurrence_info
{
    amount_t work_dup = amount_t::Unknown;

    amount_t code_dup = amount_t::Unknown;

    var_context context = var_context::unknown;

    bool is_loop_breaker = false;

    bool top_level = false;
};
    

// a dummy variable expression
struct dummy: public Object, public occurrence_info
{
    int index;

    std::string name;

    bool pre_inline() const;

    bool operator==(const dummy&) const;
    dummy* clone() const {return new dummy(*this);}
    std::string print() const;
    tribool compare(const Object& o) const;
  
    bool operator<(const dummy&) const;

    type_constant type() const {return dummy_type;}

    bool is_wildcard() const {return index < 0 and not name.size();}

    dummy(int i):index(i) {}
    dummy(const std::string& s):dummy(s,-1) {}
    dummy(const std::string& s,int i):index(i),name(s) {assert(name != "_");}
};

bool is_dummy(const expression_ref&);

bool is_wildcard(const expression_ref&);

bool is_wildcard(const dummy& d);

int max_index(const std::set<dummy>& s);

std::set<dummy> get_free_indices(const expression_ref& E);

std::set<dummy> get_bound_indices(const expression_ref& E);

int get_safe_binder_index(const expression_ref& E);

#endif
