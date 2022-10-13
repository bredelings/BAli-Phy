#ifndef CORE_H
#define CORE_H

#include <utility>  // for function
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/expression/let.H"

// make a Core namespace

namespace Core
{
    // This should be Core::Exp
    typedef expression_ref Exp;
    typedef expression_ref Pat;
    typedef CDecl Decl;
    typedef CDecls Decls;

    typedef var Var;

    // Wrappers
    typedef std::function<Exp(const Exp&)> wrapper;

    extern Core::wrapper wrapper_id;

    Exp Tuple(const std::vector<Exp>& es);
    Exp Lambda(const std::vector<Var>& args, const Exp& body);
    Exp Let(const Decls& args, const Exp& body);
    Exp Apply(const Exp& fun, const std::vector<Exp>& args);
    Exp Apply(const Exp& fun, const std::vector<Var>& args);
    Exp Case(const Exp& object, const std::vector<Pat>& patterns, const std::vector<Exp>& bodies);

    wrapper WrapLet(const Decls& args);
    wrapper WrapLambda(const std::vector<Var>& args);
    wrapper WrapApply(const std::vector<Exp>& args);
    wrapper WrapApply(const std::vector<Var>& args);
}

Core::wrapper operator*(const Core::wrapper&, const Core::wrapper&);

#endif
