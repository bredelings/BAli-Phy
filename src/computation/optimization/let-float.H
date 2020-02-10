#ifndef LET_FLOAT_H
#define LET_FLOAT_H
#include "computation/expression/expression_ref.H"

#include "object.H"      // for Box
#include "immer/set.hpp" // for immer::set
#include "computation/expression/var.H"

expression_ref let_float(const expression_ref& E);

typedef Box<immer::set<var>> FreeVarSet;

template<typename T>
struct annot_expression_ref: public Object
{
    annot_expression_ref<T>* clone() const {return new annot_expression_ref<T>(*this);}

    T note;
    expression_ref exp;

    std::string print() const {return exp.print();}

    annot_expression_ref() = default;
    annot_expression_ref(const T& t, const expression_ref& e):note(t),exp(e) {}
    annot_expression_ref(const T&& t, const expression_ref& e):note(std::move(t)),exp(e) {}
};

template <>
std::string annot_expression_ref<FreeVarSet>::print() const;

annot_expression_ref<FreeVarSet>
add_free_variable_annotations(const expression_ref& E);

expression_ref set_level(const expression_ref& E);
expression_ref float_lets(const expression_ref& E);

#endif
