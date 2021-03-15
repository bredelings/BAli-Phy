#ifndef HASKELL_H
#define HASKELL_H

#include <string>
#include <utility>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "located.H"

struct Class: public Object
{
    Located<expression_ref> class_header;
    Located<expression_ref> decls;

    Class* clone() const { return new Class(*this);}
    Class(const Located<expression_ref>& ch, const Located<expression_ref>& ds): class_header(ch), decls(ds) {}
};

struct HList: public Object
{
    std::vector<expression_ref> elements;
    HList* clone() const { return new HList(*this);}
    std::string print() const;
    HList(const std::vector<expression_ref>& es): elements(es) {}
};

struct HTuple: public Object
{
    std::vector<expression_ref> elements;
    HTuple* clone() const { return new HTuple(*this);}
    std::string print() const;
    HTuple(const std::vector<expression_ref>& es): elements(es) {}
};

typedef Located<Class> LClass;

#endif
