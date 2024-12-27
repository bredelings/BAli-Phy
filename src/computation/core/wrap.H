#ifndef CORE_WRAP_H
#define CORE_WRAP_H

#include "ast.H"

namespace Core2
{
    // Wrappers
    struct WrapObj
    {
        virtual WrapObj* clone() const = 0;
        virtual Exp<> operator()(const Exp<>&) const = 0;
        std::string print() const;
        virtual ~WrapObj() = default;
    };

    class wrapper
    {
        std::shared_ptr<WrapObj> ptr;
    public:

        Exp<> operator()(const Exp<>& e) const;
        std::string print() const;

        wrapper& operator=(const wrapper&) = default;
        wrapper& operator=(wrapper&&) = default;

        bool is_identity() const;

        wrapper() = default;
        wrapper(const wrapper&) = default;
        wrapper(wrapper&&) = default;

        wrapper(const WrapObj&);
    };

    wrapper WrapLet(const Decls<>& d);
    wrapper WrapLet(const std::shared_ptr<const Decls<>>& d);
    wrapper WrapApply(const std::vector<Exp<>>& args);
    wrapper WrapApply(const std::vector<Var<>>& args);
    wrapper WrapLambda(const std::vector<Var<>>& args);

    extern wrapper WrapId;
};

Core2::wrapper operator*(const Core2::wrapper&, const Core2::wrapper&);
#endif
