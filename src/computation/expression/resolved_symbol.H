#ifndef RESOLVED_SYMBOL_H
#define RESOLVED_SYMBOL_H

#include <set>
#include <string>
#include "computation/object.H"
#include "expression_ref.H"

// a variable expression
struct resolved_symbol: public Object
{
    std::string name;

    bool operator==(const resolved_symbol&) const;
    bool operator==(const Object& o) const;
    resolved_symbol* clone() const {return new resolved_symbol(*this);}
    std::string print() const;

    bool operator<(const resolved_symbol&) const;

    resolved_symbol(const std::string& s):name(s) {}
};

bool is_resolved_symbol(const expression_ref& E);
#endif
