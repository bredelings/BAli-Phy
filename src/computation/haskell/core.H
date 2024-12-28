#ifndef CORE_H
#define CORE_H

#include <utility>  // for function
#include <memory>   // for shared_ptr
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/expression/let.H"
#include "computation/core/ast.H"


class FreshVarSource;

namespace Core
{
    // This should be Core::Exp
    typedef expression_ref Exp;
    typedef expression_ref Pat;
    typedef CDecl Decl;
    typedef CDecls Decls;

    typedef var Var;

    Exp ConExp(const std::string& name, const std::vector<Exp>& es);
    Exp Tuple(const std::vector<Exp>& es);
    Exp Lambda(const std::vector<Var>& args, const Exp& body);
    Exp Let(const Decls& args, const Exp& body);
    Exp Apply(const Exp& fun, const std::vector<Exp>& args);
    Exp Apply(const Exp& fun, const std::vector<Var>& args);
    Exp Case(const Exp& object, const std::vector<Pat>& patterns, const std::vector<Exp>& bodies);

    std::tuple<Decls, std::vector<Exp>>
    args_to_vars(const std::vector<Exp>& args, FreshVarSource&);

    Exp safe_apply(const Exp&, const std::vector<Exp>& args, FreshVarSource&);
}

// Core should have:

// Exp = 
//   Var
//   Lambda Var Exp
//   Apply Exp Exp
//   Let Decl Exp
//   Case Exp of [Alt]
//   Constructor Var*
//   Builtin-object
//   Builtin-operation Var*

// Decl = [(Var,Exp)]

// Alt = [(Pat,Exp)]

// Pat =
//   _
//   Con Var*

// But -- at some points we need to replace expressions with annot_expression_ref<T> = (T,expression_ref).
// We might also need to change what an ID is for a var?
#endif
