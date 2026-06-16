#include "models/model-expr-ptree.H"

#include <optional>
#include <stdexcept>
#include <string>

using std::string;

namespace
{

bool is_quoted_string(const string& s)
{
    return s.size() >= 2 and s.front() == '"' and s.back() == '"';
}

string strip_quotes(const string& s)
{
    return s.substr(1, s.size() - 2);
}

string add_quotes(const string& s)
{
    return "\"" + s + "\"";
}

void require_child_count(const ptree& p, int n, const string& name)
{
    if (p.children().size() != n)
        throw std::logic_error("malformed model ptree for " + name);
}

UntypedModelExpr model_expr_from_string_ptree(const ptree& p)
{
    auto name = p.get_value<string>();

    if (p.children().empty())
    {
        if (name == "_")
            return {UntypedAnn{}, Placeholder{}};
        else if (not name.empty() and name[0] == '@')
            return {UntypedAnn{}, ArgRef{name.substr(1)}};
        else if (is_quoted_string(name))
            return {UntypedAnn{}, StringLiteral{strip_quotes(name)}};
        else
            return {UntypedAnn{}, Var{name}};
    }

    if (name == "List")
    {
        List<UntypedAnn> list;
        for(auto& [key, value]: p.children())
            list.elements.push_back(model_expr_from_ptree(value));
        return {UntypedAnn{}, std::move(list)};
    }
    else if (name == "Tuple")
    {
        Tuple<UntypedAnn> tuple;
        for(auto& [key, value]: p.children())
            tuple.elements.push_back(model_expr_from_ptree(value));
        if (tuple.elements.size() < 2)
            throw std::logic_error("malformed model ptree for Tuple");
        return {UntypedAnn{}, std::move(tuple)};
    }
    else if (name == "!let")
    {
        if (p.count("decls") != 1 or p.count("body") != 1 or p.children().size() != 2)
            throw std::logic_error("malformed model ptree for !let");

        auto decls = model_decls_from_ptree(p.get_child("decls"));
        auto body = model_expr_from_ptree(p.get_child("body"));
        return {UntypedAnn{}, Let<UntypedAnn>{std::move(decls), Box<UntypedModelExpr>(std::move(body))}};
    }
    else if (name == "function")
    {
        require_child_count(p, 2, "function");
        auto pattern = model_expr_from_ptree(p.children()[0].second);
        auto body = model_expr_from_ptree(p.children()[1].second);
        return {UntypedAnn{}, Lambda<UntypedAnn>{Box<UntypedModelExpr>(std::move(pattern)), Box<UntypedModelExpr>(std::move(body))}};
    }
    else if (name == "sample")
    {
        require_child_count(p, 1, "sample");
        auto dist = model_expr_from_ptree(p.children()[0].second);
        return {UntypedAnn{}, Sample<UntypedAnn>{Box<UntypedModelExpr>(std::move(dist))}};
    }
    else if (name == "get_state")
    {
        require_child_count(p, 1, "get_state");
        auto& state = p.children()[0].second;
        if (not state.is_a<string>())
            throw std::logic_error("malformed model ptree for get_state");
        return {UntypedAnn{}, GetState{state.get_value<string>()}};
    }
    else
    {
        Call<UntypedAnn> call{name, {}};
        for(auto& [key, value]: p.children())
            call.args.push_back({key, Box<UntypedModelExpr>(model_expr_from_ptree(value)), false, false, std::nullopt});
        return {UntypedAnn{}, std::move(call)};
    }
}

}

UntypedModelExpr model_expr_from_ptree(const ptree& p)
{
    if (p.is_null())
        return {UntypedAnn{}, MissingArg{}};
    else if (p.is_a<int>())
        return {UntypedAnn{}, IntLiteral{p.get_value<int>()}};
    else if (p.is_a<double>())
        return {UntypedAnn{}, DoubleLiteral{p.get_value<double>()}};
    else if (p.is_a<bool>())
        return {UntypedAnn{}, BoolLiteral{p.get_value<bool>()}};
    else if (p.has_value<string>())
        return model_expr_from_string_ptree(p);
    else
        throw std::logic_error("malformed model ptree: non-string value has children");
}

ptree ptree_from_model_expr(const UntypedModelExpr& expr)
{
    return std::visit(overloaded{
        [](const IntLiteral& x) -> ptree {return ptree(x.value);},
        [](const DoubleLiteral& x) -> ptree {return ptree(x.value);},
        [](const BoolLiteral& x) -> ptree {return ptree(x.value);},
        [](const StringLiteral& x) -> ptree {return ptree(add_quotes(x.value));},
        [](const Var& x) -> ptree {return ptree(x.name);},
        [](const ArgRef& x) -> ptree {return ptree("@" + x.name);},
        [](const Placeholder&) -> ptree {return ptree("_");},
        [](const MissingArg&) -> ptree {return ptree();},
        [](const GetState& x) -> ptree {return ptree("get_state", {{"", ptree(x.state_name)}});},
        [](const Call<UntypedAnn>& x) -> ptree
        {
            ptree result(x.function);
            for(auto& arg: x.args)
                result.children().push_back({arg.name, ptree_from_model_expr(arg.value.get())});
            return result;
        },
        [](const List<UntypedAnn>& x) -> ptree
        {
            ptree result("List");
            for(auto& element: x.elements)
                result.children().push_back({"", ptree_from_model_expr(element)});
            return result;
        },
        [](const Tuple<UntypedAnn>& x) -> ptree
        {
            if (x.elements.size() < 2)
                throw std::logic_error("cannot convert one-element model tuple to ptree");
            ptree result("Tuple");
            for(auto& element: x.elements)
                result.children().push_back({"", ptree_from_model_expr(element)});
            return result;
        },
        [](const Let<UntypedAnn>& x) -> ptree
        {
            return ptree("!let", {
                {"decls", ptree_from_model_decls(x.decls)},
                {"body", ptree_from_model_expr(x.body.get())}
            });
        },
        [](const Lambda<UntypedAnn>& x) -> ptree
        {
            return ptree("function", {
                {"", ptree_from_model_expr(x.pattern.get())},
                {"", ptree_from_model_expr(x.body.get())}
            });
        },
        [](const Sample<UntypedAnn>& x) -> ptree
        {
            return ptree("sample", {{"", ptree_from_model_expr(x.dist.get())}});
        }
    }, expr.node);
}

ModelDecls<UntypedAnn> model_decls_from_ptree(const ptree& p)
{
    if (not p.has_value<string>() or p.get_value<string>() != "!Decls")
        throw std::logic_error("malformed model ptree for declarations");

    ModelDecls<UntypedAnn> decls;
    for(auto& [name, value]: p.children())
        decls.push_back({name, model_expr_from_ptree(value)});
    return decls;
}

ptree ptree_from_model_decls(const ModelDecls<UntypedAnn>& decls)
{
    ptree result("!Decls");
    for(auto& [name, value]: decls)
        result.children().push_back({name, ptree_from_model_expr(value)});
    return result;
}
