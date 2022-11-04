#ifndef CORE_H
#define CORE_H

#include <utility>  // for function
#include <memory>   // for shared_ptr
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
    wrapper WrapApply(const std::vector<Exp>& args);
    wrapper WrapApply(const std::vector<Var>& args);
    wrapper WrapLambda(const std::vector<Var>& args);

    extern wrapper WrapId;

    Exp Tuple(const std::vector<Exp>& es);
    Exp Lambda(const std::vector<Var>& args, const Exp& body);
    Exp Let(const Decls& args, const Exp& body);
    Exp Let(const std::shared_ptr<const Decls>& args, const Exp& body);
    Exp Apply(const Exp& fun, const std::vector<Exp>& args);
    Exp Apply(const Exp& fun, const std::vector<Var>& args);
    Exp Case(const Exp& object, const std::vector<Pat>& patterns, const std::vector<Exp>& bodies);
}

Core::wrapper operator*(const Core::wrapper&, const Core::wrapper&);

#endif
