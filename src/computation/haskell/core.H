#ifndef CORE_H
#define CORE_H

#include <utility>  // for function
#include <memory>   // for shared_ptr
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/expression/let.H"

class FreshVarSource;

namespace Core
{
    // This should be Core::Exp
    typedef expression_ref Exp;
    typedef expression_ref Pat;
    typedef CDecl Decl;
    typedef CDecls Decls;

    typedef var Var;

    // Wrappers
    struct WrapObj
    {
        virtual WrapObj* clone() const = 0;
        virtual Exp operator()(const Exp&) const = 0;
        std::string print() const;
        virtual ~WrapObj() = default;
    };

    class wrapper
    {
        std::shared_ptr<WrapObj> ptr;
    public:

        Exp operator()(const Exp& e) const;
        std::string print() const;

        wrapper& operator=(const wrapper&) = default;
        wrapper& operator=(wrapper&&) = default;

        bool is_identity() const;

        wrapper() = default;
        wrapper(const wrapper&) = default;
        wrapper(wrapper&&) = default;

        wrapper(const WrapObj&);
    };

    wrapper WrapLet(const Decls& d);
    wrapper WrapLet(const std::shared_ptr<const Decls>& d);
    wrapper WrapApply(const std::vector<Var>& args);
    wrapper WrapLambda(const std::vector<Var>& args);

    extern wrapper WrapId;

    Exp ConExp(const std::string& name, const std::vector<Exp>& es);
    Exp Tuple(const std::vector<Exp>& es);
    Exp Lambda(const std::vector<Var>& args, const Exp& body);
    Exp Let(const Decls& args, const Exp& body);
    Exp Let(const std::shared_ptr<const Decls>& args, const Exp& body);
    Exp Apply(const Exp& fun, const std::vector<Exp>& args);
    Exp Apply(const Exp& fun, const std::vector<Var>& args);
    Exp Case(const Exp& object, const std::vector<Pat>& patterns, const std::vector<Exp>& bodies);

    std::tuple<Decls, std::vector<Exp>>
    args_to_vars(const std::vector<Exp>& args, FreshVarSource&);

    Exp safe_apply(const Exp&, const std::vector<Exp>& args, FreshVarSource&);
}

Core::wrapper operator*(const Core::wrapper&, const Core::wrapper&);

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
