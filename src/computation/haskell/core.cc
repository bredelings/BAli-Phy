#include "core.H"

#include "expression/tuple.H"
#include "expression/apply.H"
#include "expression/lambda.H"
#include "expression/case.H"

#include <fstream>
#include <cereal/archives/binary.hpp>

using std::vector;

namespace Core
{

    // Expression stuff
    Exp ConExp(const std::string& name, const std::vector<Exp>& es)
    {
        return expression_ref{constructor(name,es.size()), es};
    }

    Exp Tuple(const std::vector<Exp>& es)
    {
        return get_tuple(es);
    }

    Exp Lambda(const std::vector<Var>& args, const Exp& body)
    {
        if (args.empty())
            return body;
        else
            return lambda_quantify(args, body);
    }

    Exp Let(const Decls& decls, const Exp& body)
    {
        if (decls.empty())
            return body;
        else
            return let_expression(decls, body);
    }

    Exp Let(const std::shared_ptr<const Decls>& decls, const Exp& body)
    {
        if (decls->empty())
            return body;
        else
            return let_expression(*decls, body);
    }

    Exp Apply(const Exp& fun, const std::vector<Exp>& args)
    {
        if (args.empty())
            return fun;
        else
            return apply_expression(fun, args);
    }

    Exp Apply(const Exp& fun, const std::vector<Var>& args)
    {
        if (args.empty())
            return fun;

        vector<Exp> args2;
        for(auto& arg: args)
            args2.push_back(arg);

        return apply_expression(fun, args2);
    }

    Exp Case(const Exp& object, const std::vector<Pat>& patterns, const std::vector<Exp>& bodies)
    {
        return make_case_expression(object, patterns, bodies);
    }

    bool wrapper::is_identity() const
    {
        return not ptr;
    }

    // wrapper stuff
    Exp wrapper::operator()(const Exp& e) const
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
        expression_ref p = operator()(var(-1));
        return p.print();
    }

    struct WrapLetObj: public WrapObj
    {
        std::shared_ptr<const Decls> decls;

        WrapLetObj* clone() const {return new WrapLetObj(*this);}
        Exp operator()(const Exp&) const;

        WrapLetObj(const Decls& d);
        WrapLetObj(const std::shared_ptr<const Decls>& d);
    };

    Exp WrapLetObj::operator()(const Exp& e) const
    {
        return Let(decls, e);
    }

    WrapLetObj::WrapLetObj(const Decls& d)
        :decls(std::make_shared<const Decls>(d))
    {}

    WrapLetObj::WrapLetObj(const std::shared_ptr<const Decls>& d)
        :decls(d)
    {}

    wrapper WrapLet(const Decls& d)
    {
        if (d.empty())
            return {};
        else
            return WrapLetObj(d);
    }

    wrapper WrapLet(const std::shared_ptr<const Decls>& d)
    {
        // We can't return an identify here, because the decls might grow, later.
        return WrapLetObj(d);
    }

    struct WrapApplyObj: public WrapObj
    {
        std::vector<Var> args;

        WrapApplyObj* clone() const {return new WrapApplyObj(*this);}
        Exp operator()(const Exp&) const;

        WrapApplyObj(const std::vector<Var>& args);
    };

    Exp WrapApplyObj::operator()(const Exp& e) const
    {
        return Apply(e, args);
    }

    WrapApplyObj::WrapApplyObj(const vector<Var>& as)
    {
        for(auto& a:as)
            args.push_back(a);
    }

    wrapper WrapApply(const std::vector<Var>& args)
    {
        if (args.empty())
            return {};
        else
            return {WrapApplyObj(args)};
    }

    struct WrapLambdaObj: public WrapObj
    {
        std::vector<Var> args;

        WrapLambdaObj* clone() const {return new WrapLambdaObj(*this);}
        Exp operator()(const Exp&) const;

        WrapLambdaObj(const std::vector<Var>& args);
    };

    Exp WrapLambdaObj::operator()(const Exp& e) const
    {
        return Lambda(args, e);
    }

    WrapLambdaObj::WrapLambdaObj(const vector<Var>& as):
        args(as)
    {}

    wrapper WrapLambda(const std::vector<Var>& args)
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
        Exp operator()(const Exp&) const;

        WrapCompose(const wrapper& W1, const wrapper& W2);
    };

    Exp WrapCompose::operator()(const Exp& e) const
    {
        return w1(w2(e));
    }

    WrapCompose::WrapCompose(const wrapper& W1, const wrapper& W2)
        :w1(W1), w2(W2)
    {}
}


Core::wrapper operator*(const Core::wrapper& w1, const Core::wrapper& w2)
{
    if (w1.is_identity())
        return w2;
    else if (w2.is_identity())
        return w1;
    else
        return {Core::WrapCompose(w1,w2)};
}
