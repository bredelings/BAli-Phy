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

std::set<string> used_args_from_ptree(const ptree& p)
{
    std::set<string> used_args;
    if (auto used = p.get_child_optional("used_args"))
        for(auto& [key, value]: used->children())
            used_args.insert(value.get_value<string>());
    return used_args;
}

ptree used_args_to_ptree(const std::set<string>& used_args)
{
    ptree result;
    for(auto& arg: used_args)
        result.children().push_back({"", ptree(arg)});
    return result;
}

ModelAnn ann_from_ptree(const ptree& p)
{
    ModelAnn ann;
    ann.type = p.get_child("type");
    ann.used_args = used_args_from_ptree(p);
    ann.no_log = p.get("no_log", false);
    if (auto extract = p.get_optional<string>("extract"))
        ann.extract = *extract;
    return ann;
}

void add_ann_to_ptree(ptree& p, const ModelAnn& ann)
{
    p.children().push_back({"type", ann.type});
    p.children().push_back({"used_args", used_args_to_ptree(ann.used_args)});
    if (ann.no_log)
        p.children().push_back({"no_log", ptree(true)});
    if (ann.extract)
        p.children().push_back({"extract", ptree(*ann.extract)});
}

ModelArg<ModelAnn> typed_model_arg_from_annotated_ptree(const string& name, const ptree& p)
{
    auto value = typed_model_expr_from_annotated_ptree(p);
    ModelArg<ModelAnn> arg{name, Box<TypedModelExpr>(std::move(value)), false, false, std::nullopt};
    arg.is_default_value = p.get("is_default_value", false);
    arg.suppress_default = p.get("suppress_default", false);
    if (auto alphabet = p.get_child_optional("alphabet"))
        arg.alphabet = Box<TypedModelExpr>(typed_model_expr_from_annotated_ptree(*alphabet));
    return arg;
}

void add_arg_metadata(ptree& p, const ModelArg<ModelAnn>& arg)
{
    p.children().push_back({"is_default_value", ptree(arg.is_default_value)});
    if (arg.suppress_default)
        p.children().push_back({"suppress_default", ptree(true)});
    if (arg.alphabet)
        p.children().push_back({"alphabet", annotated_ptree_from_typed_model_expr(arg.alphabet->get())});
}

TypedModelExpr typed_model_expr_from_value_ptree(ModelAnn ann, const ptree& value)
{
    if (value.is_null())
        return {std::move(ann), MissingArg{}};
    else if (value.is_a<int>())
        return {std::move(ann), IntLiteral{value.get_value<int>()}};
    else if (value.is_a<double>())
        return {std::move(ann), DoubleLiteral{value.get_value<double>()}};
    else if (value.is_a<bool>())
        return {std::move(ann), BoolLiteral{value.get_value<bool>()}};
    else if (not value.has_value<string>())
        throw std::logic_error("malformed annotated model ptree: non-string value has children");

    auto name = value.get_value<string>();

    if (value.children().empty())
    {
        if (name == "_")
            return {std::move(ann), Placeholder{}};
        else if (not name.empty() and name[0] == '@')
            return {std::move(ann), ArgRef{name.substr(1)}};
        else if (is_quoted_string(name))
            return {std::move(ann), StringLiteral{strip_quotes(name)}};
        else
            return {std::move(ann), Var{name}};
    }

    if (name == "List")
    {
        List<ModelAnn> list;
        for(auto& [key, child]: value.children())
            list.elements.push_back(typed_model_expr_from_annotated_ptree(child));
        return {std::move(ann), std::move(list)};
    }
    else if (name == "Tuple")
    {
        Tuple<ModelAnn> tuple;
        for(auto& [key, child]: value.children())
            tuple.elements.push_back(typed_model_expr_from_annotated_ptree(child));
        if (tuple.elements.size() < 2)
            throw std::logic_error("malformed annotated model ptree for Tuple");
        return {std::move(ann), std::move(tuple)};
    }
    else if (name == "!let")
    {
        if (value.count("decls") != 1 or value.count("body") != 1 or value.children().size() != 2)
            throw std::logic_error("malformed annotated model ptree for !let");

        auto decls = typed_model_decls_from_annotated_ptree(value.get_child("decls"));
        auto body = typed_model_expr_from_annotated_ptree(value.get_child("body"));
        return {std::move(ann), Let<ModelAnn>{std::move(decls), Box<TypedModelExpr>(std::move(body))}};
    }
    else if (name == "function")
    {
        require_child_count(value, 2, "function");
        auto pattern = typed_model_expr_from_annotated_ptree(value.children()[0].second);
        auto body = typed_model_expr_from_annotated_ptree(value.children()[1].second);
        return {std::move(ann), Lambda<ModelAnn>{Box<TypedModelExpr>(std::move(pattern)), Box<TypedModelExpr>(std::move(body))}};
    }
    else if (name == "sample")
    {
        require_child_count(value, 1, "sample");
        auto dist = typed_model_expr_from_annotated_ptree(value.children()[0].second);
        return {std::move(ann), Sample<ModelAnn>{Box<TypedModelExpr>(std::move(dist))}};
    }
    else if (name == "get_state")
    {
        require_child_count(value, 1, "get_state");
        auto& state = value.children()[0].second;
        if (state.has_value<string>() and state.is_a<string>())
            return {std::move(ann), GetState{state.get_value<string>()}};
        auto state_expr = typed_model_expr_from_annotated_ptree(state);
        if (auto var = std::get_if<Var>(&state_expr.node))
            return {std::move(ann), GetState{var->name}};
        if (auto str = std::get_if<StringLiteral>(&state_expr.node))
            return {std::move(ann), GetState{str->value}};
        throw std::logic_error("malformed annotated model ptree for get_state");
    }
    else
    {
        Call<ModelAnn> call{name, {}};
        for(auto& [key, child]: value.children())
            call.args.push_back(typed_model_arg_from_annotated_ptree(key, child));
        return {std::move(ann), std::move(call)};
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

TypedModelExpr typed_model_expr_from_annotated_ptree(const ptree& p)
{
    auto ann = ann_from_ptree(p);
    return typed_model_expr_from_value_ptree(std::move(ann), p.get_child("value"));
}

ptree annotated_ptree_from_typed_model_expr(const TypedModelExpr& expr)
{
    ptree value = std::visit(overloaded{
        [](const IntLiteral& x) -> ptree {return ptree(x.value);},
        [](const DoubleLiteral& x) -> ptree {return ptree(x.value);},
        [](const BoolLiteral& x) -> ptree {return ptree(x.value);},
        [](const StringLiteral& x) -> ptree {return ptree(add_quotes(x.value));},
        [](const Var& x) -> ptree {return ptree(x.name);},
        [](const ArgRef& x) -> ptree {return ptree("@" + x.name);},
        [](const Placeholder&) -> ptree {return ptree("_");},
        [](const MissingArg&) -> ptree {return ptree();},
        [](const GetState& x) -> ptree {return ptree("get_state", {{"", annotated_ptree_from_typed_model_expr({ModelAnn{ptree("String"), {}, false, {}}, Var{x.state_name}})}});},
        [](const Call<ModelAnn>& x) -> ptree
        {
            ptree result(x.function);
            for(auto& arg: x.args)
            {
                auto value = annotated_ptree_from_typed_model_expr(arg.value.get());
                add_arg_metadata(value, arg);
                result.children().push_back({arg.name, value});
            }
            return result;
        },
        [](const List<ModelAnn>& x) -> ptree
        {
            ptree result("List");
            for(auto& element: x.elements)
                result.children().push_back({"", annotated_ptree_from_typed_model_expr(element)});
            return result;
        },
        [](const Tuple<ModelAnn>& x) -> ptree
        {
            if (x.elements.size() < 2)
                throw std::logic_error("cannot convert one-element annotated model tuple to ptree");
            ptree result("Tuple");
            for(auto& element: x.elements)
                result.children().push_back({"", annotated_ptree_from_typed_model_expr(element)});
            return result;
        },
        [](const Let<ModelAnn>& x) -> ptree
        {
            return ptree("!let", {
                {"decls", annotated_ptree_from_typed_model_decls(x.decls)},
                {"body", annotated_ptree_from_typed_model_expr(x.body.get())}
            });
        },
        [](const Lambda<ModelAnn>& x) -> ptree
        {
            return ptree("function", {
                {"", annotated_ptree_from_typed_model_expr(x.pattern.get())},
                {"", annotated_ptree_from_typed_model_expr(x.body.get())}
            });
        },
        [](const Sample<ModelAnn>& x) -> ptree
        {
            return ptree("sample", {{"", annotated_ptree_from_typed_model_expr(x.dist.get())}});
        }
    }, expr.node);

    ptree result({{"value", value}});
    add_ann_to_ptree(result, expr.ann);
    return result;
}

TypedModelDecls typed_model_decls_from_annotated_ptree(const ptree& p)
{
    if (not p.has_value<string>() or p.get_value<string>() != "!Decls")
        throw std::logic_error("malformed annotated model ptree for declarations");

    TypedModelDecls decls;
    for(auto& [name, value]: p.children())
        decls.push_back({name, typed_model_expr_from_annotated_ptree(value)});
    return decls;
}

ptree annotated_ptree_from_typed_model_decls(const TypedModelDecls& decls)
{
    ptree result("!Decls");
    for(auto& [name, value]: decls)
        result.children().push_back({name, annotated_ptree_from_typed_model_expr(value)});
    return result;
}
