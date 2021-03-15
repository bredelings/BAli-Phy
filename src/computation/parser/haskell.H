#ifndef HASKELL_H
#define HASKELL_H

#include <string>
#include <utility>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "located.H"

namespace Haskell
{

struct Class: public Object
{
    Located<expression_ref> class_header;
    Located<expression_ref> decls;

    Class* clone() const { return new Class(*this);}
    Class(const Located<expression_ref>& ch, const Located<expression_ref>& ds): class_header(ch), decls(ds) {}
};

struct List: public Object
{
    std::vector<expression_ref> elements;
    List* clone() const { return new List(*this);}
    std::string print() const;
    List(const std::vector<expression_ref>& es): elements(es) {}
};

struct Tuple: public Object
{
    std::vector<expression_ref> elements;
    Tuple* clone() const { return new Tuple(*this);}
    std::string print() const;
    Tuple(const std::vector<expression_ref>& es): elements(es) {}
};

}

#endif
