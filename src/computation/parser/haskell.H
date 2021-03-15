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

struct LetQual: public Object
{
    expression_ref binds;
    LetQual* clone() const { return new LetQual(*this);}
    std::string print()  const;
    LetQual(const expression_ref& b): binds(b) {}
};

struct SimpleQual: public Object
{
    expression_ref exp;
    SimpleQual* clone() const { return new SimpleQual(*this);}
    std::string print() const;
    SimpleQual(const expression_ref &e): exp(e) {};
};

struct PatQual: public Object
{
    expression_ref bindpat;
    expression_ref exp;
    PatQual* clone() const { return new PatQual(*this);}
    std::string print() const;
    PatQual(const expression_ref &bp, const expression_ref& e): bindpat(bp), exp(e) {};
};

}

#endif
