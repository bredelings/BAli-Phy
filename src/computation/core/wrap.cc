#include "core/wrap.H"

#include "core/func.H"
#include "computation/fresh_vars.H"

using std::vector;

namespace Core2
{
    bool wrapper::is_identity() const
    {
        return not ptr;
    }

    // wrapper stuff
    Exp<> wrapper::operator()(const Exp<>& e) const
    {
        if (ptr)
            return (*ptr)(e);
        else
            return e;
    }

    std::string wrapper::print() const
    {
        if (ptr)
            return ptr->print();
        else
            return "_";
    }

    wrapper::wrapper(const WrapObj& wo)
        :ptr(wo.clone())
    { }

    wrapper WrapId;

    std::string WrapObj::print() const
    {
        auto p = operator()(Var<>("_"));
        return p.print();
    }

    struct WrapLetObj: public WrapObj
    {
        std::shared_ptr<const Decls<>> decls;

        WrapLetObj* clone() const {return new WrapLetObj(*this);}
        Exp<> operator()(const Exp<>&) const;

        WrapLetObj(const Decls<>& d);
        WrapLetObj(const std::shared_ptr<const Decls<>>& d);
    };

    Exp<> WrapLetObj::operator()(const Exp<>& e) const
    {
        return make_let(*decls,e);
    }

    WrapLetObj::WrapLetObj(const Decls<>& d)
        :decls(std::make_shared<const Decls<>>(d))
    {}

    WrapLetObj::WrapLetObj(const std::shared_ptr<const Decls<>>& d)
        :decls(d)
    {}

    wrapper WrapLet(const Decls<>& d)
    {
        if (d.empty())
            return {};
        else
            return WrapLetObj(d);
    }

    wrapper WrapLet(const std::shared_ptr<const Decls<>>& d)
    {
        // We can't return an identify here, because the decls might grow, later.
        return WrapLetObj(d);
    }

    struct WrapApplyObj: public WrapObj
    {
        Var<> arg;

        WrapApplyObj* clone() const {return new WrapApplyObj(*this);}
        Exp<> operator()(const Exp<>&) const;

        WrapApplyObj(const Var<>& arg);
    };

    Exp<> WrapApplyObj::operator()(const Exp<>& e) const
    {
        return Apply<>{e, arg};
    }

    WrapApplyObj::WrapApplyObj(const Var<>& a)
        :arg(a)
    {
    }

    /*    
    wrapper WrapApply(const std::vector<Exp<>>& args)
    {
        if (args.empty())
            return {};
        else
            return {WrapApplyObj(args)};
    }
    */
    wrapper WrapApply(const Var<>& arg)
    {
        return {WrapApplyObj(arg)};
    }

    wrapper WrapApply(const std::vector<Var<>>& args)
    {
        wrapper w;
        for(auto& arg: args)
            w = WrapApply(arg) * w;
        return w;
    }

    struct WrapLambdaObj: public WrapObj
    {
        std::vector<Var<>> args;

        WrapLambdaObj* clone() const {return new WrapLambdaObj(*this);}
        Exp<> operator()(const Exp<>&) const;

        WrapLambdaObj(const std::vector<Var<>>& args);
    };

    Exp<> WrapLambdaObj::operator()(const Exp<>& e) const
    {
        return lambda_quantify(args, e);
    }

    WrapLambdaObj::WrapLambdaObj(const vector<Var<>>& as):
        args(as)
    {}

    wrapper WrapLambda(const std::vector<Var<>>& args)
    {
        if (args.empty())
            return WrapId;
        else
            return {WrapLambdaObj(args)};
    }
    
    struct WrapCompose: public WrapObj
    {
        wrapper w1;
        wrapper w2;

        WrapCompose* clone() const {return new WrapCompose(*this);}
        Exp<> operator()(const Exp<>&) const;

        WrapCompose(const wrapper& W1, const wrapper& W2);
    };

    Exp<> WrapCompose::operator()(const Exp<>& e) const
    {
        return w1(w2(e));
    }

    WrapCompose::WrapCompose(const wrapper& W1, const wrapper& W2)
        :w1(W1), w2(W2)
    {}
}


Core2::wrapper operator*(const Core2::wrapper& w1, const Core2::wrapper& w2)
{
    if (w1.is_identity())
        return w2;
    else if (w2.is_identity())
        return w1;
    else
        return {Core2::WrapCompose(w1,w2)};
}
