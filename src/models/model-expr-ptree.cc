#include "models/model-expr-ptree.H"

#include <optional>
#include <stdexcept>
#include <string>

using std::string;

namespace CmdModel
{

namespace
{

// Reports whether a ptree string value carries model-string quote markers.
bool is_quoted_string(const string& s)
{
    return s.size() >= 2 and s.front() == '"' and s.back() == '"';
}

// Removes the surrounding model-string quote markers from a ptree value.
string strip_quotes(const string& s)
{
    return s.substr(1, s.size() - 2);
}

// Adds model-string quote markers before storing a string literal in ptree.
string add_quotes(const string& s)
{
    return "\"" + s + "\"";
}

// Verifies that a special-form ptree has exactly the expected arity.
void require_child_count(const ptree& p, int n, const string& name)
{
    if (p.children().size() != n)
        throw std::logic_error("malformed model ptree for " + name);
}

// Converts a string-valued model ptree into the corresponding untyped AST node,
// handling special forms before falling back to ordinary calls.
UntypedExpr model_expr_from_string_ptree(const ptree& p)
{
    auto name = p.get_value<string>();

    if (p.children().empty())
    {
        if (name == "_")
            return {NoAnn{}, Placeholder{}};
        else if (not name.empty() and name[0] == '@')
            return {NoAnn{}, ArgRef{name.substr(1)}};
        else if (is_quoted_string(name))
            return {NoAnn{}, StringLiteral{strip_quotes(name)}};
        else
            return {NoAnn{}, Var{name}};
    }

    if (name == "List")
    {
        List<NoAnn> list;
        for(auto& [key, value]: p.children())
            list.elements.push_back(model_expr_from_ptree(value));
        return {NoAnn{}, std::move(list)};
    }
    else if (name == "Tuple")
    {
        Tuple<NoAnn> tuple;
        for(auto& [key, value]: p.children())
            tuple.elements.push_back(model_expr_from_ptree(value));
        if (tuple.elements.size() < 2)
            throw std::logic_error("malformed model ptree for Tuple");
        return {NoAnn{}, std::move(tuple)};
    }
    else if (name == "!let")
    {
        if (p.count("decls") != 1 or p.count("body") != 1 or p.children().size() != 2)
            throw std::logic_error("malformed model ptree for !let");

        auto decls = model_decls_from_ptree(p.get_child("decls"));
        auto body = model_expr_from_ptree(p.get_child("body"));
        return {NoAnn{}, Let<NoAnn>{std::move(decls), Box<UntypedExpr>(std::move(body))}};
    }
    else if (name == "function")
    {
        require_child_count(p, 2, "function");
        auto pattern = model_expr_from_ptree(p.children()[0].second);
        auto body = model_expr_from_ptree(p.children()[1].second);
        return {NoAnn{}, Lambda<NoAnn>{Box<UntypedExpr>(std::move(pattern)), Box<UntypedExpr>(std::move(body))}};
    }
    else if (name == "sample")
    {
        require_child_count(p, 1, "sample");
        auto dist = model_expr_from_ptree(p.children()[0].second);
        return {NoAnn{}, Sample<NoAnn>{Box<UntypedExpr>(std::move(dist))}};
    }
    else if (name == "get_state")
    {
        require_child_count(p, 1, "get_state");
        auto& state = p.children()[0].second;
        if (not state.is_a<string>())
            throw std::logic_error("malformed model ptree for get_state");
        return {NoAnn{}, GetState{state.get_value<string>()}};
    }
    else
    {
        Call<NoAnn> call{name, {}};
        for(auto& [key, value]: p.children())
        {
            std::optional<Box<UntypedExpr>> arg_value;
            if (not value.is_null())
                arg_value = Box<UntypedExpr>(model_expr_from_ptree(value));
            call.args.push_back({key, std::move(arg_value), false, false, std::nullopt});
        }
        return {NoAnn{}, std::move(call)};
    }
}

// Reads the optional used_args annotation set from an annotated ptree.
std::set<string> used_args_from_ptree(const ptree& p)
{
    std::set<string> used_args;
    if (auto used = p.get_child_optional("used_args"))
        for(auto& [key, value]: used->children())
            used_args.insert(value.get_value<string>());
    return used_args;
}

// Serializes an annotation used_args set into the legacy ptree shape.
ptree used_args_to_ptree(const std::set<string>& used_args)
{
    ptree result;
    for(auto& arg: used_args)
        result.children().push_back({"", ptree(arg)});
    return result;
}

// Builds a typed AST annotation from the metadata fields on an annotated ptree.
Ann ann_from_ptree(const ptree& p)
{
    Ann ann;
    ann.type = p.get_child("type");
    ann.used_args = used_args_from_ptree(p);
    ann.no_log = p.get("no_log", false);
    if (auto extract = p.get_optional<string>("extract"))
        ann.extract = *extract;
    return ann;
}

// Writes typed AST annotation metadata back onto an annotated ptree.
void add_ann_to_ptree(ptree& p, const Ann& ann)
{
    p.children().push_back({"type", ann.type});
    p.children().push_back({"used_args", used_args_to_ptree(ann.used_args)});
    if (ann.no_log)
        p.children().push_back({"no_log", ptree(true)});
    if (ann.extract)
        p.children().push_back({"extract", ptree(*ann.extract)});
}

// Converts one annotated ptree argument edge, preserving absent values and
// argument metadata such as defaults and alphabets.
Arg<Ann> typed_model_arg_from_annotated_ptree(const string& name, const ptree& p)
{
    std::optional<Box<TypedExpr>> value;
    if (not p.is_null())
        value = Box<TypedExpr>(typed_model_expr_from_annotated_ptree(p));
    Arg<Ann> arg{name, std::move(value), false, false, std::nullopt};
    arg.is_default_value = p.get("is_default_value", false);
    arg.suppress_default = p.get("suppress_default", false);
    if (auto alphabet = p.get_child_optional("alphabet"))
        arg.alphabet = Box<TypedExpr>(typed_model_expr_from_annotated_ptree(*alphabet));
    return arg;
}

// Writes typed call-argument metadata onto a non-null annotated argument ptree.
void add_arg_metadata(ptree& p, const Arg<Ann>& arg)
{
    p.children().push_back({"is_default_value", ptree(arg.is_default_value)});
    if (arg.suppress_default)
        p.children().push_back({"suppress_default", ptree(true)});
    if (arg.alphabet)
        p.children().push_back({"alphabet", annotated_ptree_from_typed_model_expr(arg.alphabet->get())});
}

// Converts the value child of an annotated ptree into a typed AST expression,
// combining it with the annotation already read from the parent.
TypedExpr typed_model_expr_from_value_ptree(Ann ann, const ptree& value)
{
    if (value.is_null())
        throw std::logic_error("null annotated model ptree is not an expression");
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
        List<Ann> list;
        for(auto& [key, child]: value.children())
            list.elements.push_back(typed_model_expr_from_annotated_ptree(child));
        return {std::move(ann), std::move(list)};
    }
    else if (name == "Tuple")
    {
        Tuple<Ann> tuple;
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
        return {std::move(ann), Let<Ann>{std::move(decls), Box<TypedExpr>(std::move(body))}};
    }
    else if (name == "function")
    {
        require_child_count(value, 2, "function");
        auto pattern = typed_model_expr_from_annotated_ptree(value.children()[0].second);
        auto body = typed_model_expr_from_annotated_ptree(value.children()[1].second);
        return {std::move(ann), Lambda<Ann>{Box<TypedExpr>(std::move(pattern)), Box<TypedExpr>(std::move(body))}};
    }
    else if (name == "sample")
    {
        require_child_count(value, 1, "sample");
        auto& dist_value = value.count("dist") ? value.get_child("dist") : value.children()[0].second;
        auto dist = typed_model_expr_from_annotated_ptree(dist_value);
        return {std::move(ann), Sample<Ann>{Box<TypedExpr>(std::move(dist))}};
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
        Call<Ann> call{name, {}};
        for(auto& [key, child]: value.children())
            call.args.push_back(typed_model_arg_from_annotated_ptree(key, child));
        return {std::move(ann), std::move(call)};
    }
}

}

// Converts a legacy untyped model ptree into the explicit model AST.
UntypedExpr model_expr_from_ptree(const ptree& p)
{
    if (p.is_null())
        throw std::logic_error("null model ptree is not an expression");
    else if (p.is_a<int>())
        return {NoAnn{}, IntLiteral{p.get_value<int>()}};
    else if (p.is_a<double>())
        return {NoAnn{}, DoubleLiteral{p.get_value<double>()}};
    else if (p.is_a<bool>())
        return {NoAnn{}, BoolLiteral{p.get_value<bool>()}};
    else if (p.has_value<string>())
        return model_expr_from_string_ptree(p);
    else
        throw std::logic_error("malformed model ptree: non-string value has children");
}

// Converts an untyped model AST expression back into the legacy ptree shape.
ptree ptree_from_model_expr(const UntypedExpr& expr)
{
    return std::visit(overloaded{
        [](const IntLiteral& x) -> ptree {return ptree(x.value);},
        [](const DoubleLiteral& x) -> ptree {return ptree(x.value);},
        [](const BoolLiteral& x) -> ptree {return ptree(x.value);},
        [](const StringLiteral& x) -> ptree {return ptree(add_quotes(x.value));},
        [](const Var& x) -> ptree {return ptree(x.name);},
        [](const ArgRef& x) -> ptree {return ptree("@" + x.name);},
        [](const Placeholder&) -> ptree {return ptree("_");},
        [](const GetState& x) -> ptree {return ptree("get_state", {{"", ptree(x.state_name)}});},
        // Converts an untyped AST call, preserving absent argument values as
        // null ptree children.
        [](const Call<NoAnn>& x) -> ptree
        {
            ptree result(x.function);
            for(auto& arg: x.args)
            {
                ptree value;
                if (arg.value)
                    value = ptree_from_model_expr(arg.value->get());
                result.children().push_back({arg.name, value});
            }
            return result;
        },
        // Converts each untyped list element to its positional ptree child.
        [](const List<NoAnn>& x) -> ptree
        {
            ptree result("List");
            for(auto& element: x.elements)
                result.children().push_back({"", ptree_from_model_expr(element)});
            return result;
        },
        // Converts each untyped tuple element after checking tuple arity.
        [](const Tuple<NoAnn>& x) -> ptree
        {
            if (x.elements.size() < 2)
                throw std::logic_error("cannot convert one-element model tuple to ptree");
            ptree result("Tuple");
            for(auto& element: x.elements)
                result.children().push_back({"", ptree_from_model_expr(element)});
            return result;
        },
        // Converts an untyped let expression to the legacy !let shape.
        [](const Let<NoAnn>& x) -> ptree
        {
            return ptree("!let", {
                {"decls", ptree_from_model_decls(x.decls)},
                {"body", ptree_from_model_expr(x.body.get())}
            });
        },
        // Converts an untyped lambda to the legacy function shape.
        [](const Lambda<NoAnn>& x) -> ptree
        {
            return ptree("function", {
                {"", ptree_from_model_expr(x.pattern.get())},
                {"", ptree_from_model_expr(x.body.get())}
            });
        },
        // Converts an untyped sample node to the legacy sample call shape.
        [](const Sample<NoAnn>& x) -> ptree
        {
            return ptree("sample", {{"", ptree_from_model_expr(x.dist.get())}});
        }
    }, expr.node);
}

// Converts a legacy !Decls ptree into untyped AST declarations.
Decls<NoAnn> model_decls_from_ptree(const ptree& p)
{
    if (not p.has_value<string>() or p.get_value<string>() != "!Decls")
        throw std::logic_error("malformed model ptree for declarations");

    Decls<NoAnn> decls;
    for(auto& [name, value]: p.children())
        decls.push_back({name, model_expr_from_ptree(value)});
    return decls;
}

// Converts untyped AST declarations back into the legacy !Decls ptree shape.
ptree ptree_from_model_decls(const Decls<NoAnn>& decls)
{
    ptree result("!Decls");
    for(auto& [name, value]: decls)
        result.children().push_back({name, ptree_from_model_expr(value)});
    return result;
}

// Converts a full annotated ptree expression into a typed AST expression.
TypedExpr typed_model_expr_from_annotated_ptree(const ptree& p)
{
    auto ann = ann_from_ptree(p);
    return typed_model_expr_from_value_ptree(std::move(ann), p.get_child("value"));
}

// Converts a typed AST expression back into the annotated ptree shape consumed
// by legacy code generation and pretty-printing.
ptree annotated_ptree_from_typed_model_expr(const TypedExpr& expr)
{
    ptree value = std::visit(overloaded{
        [](const IntLiteral& x) -> ptree {return ptree(x.value);},
        [](const DoubleLiteral& x) -> ptree {return ptree(x.value);},
        [](const BoolLiteral& x) -> ptree {return ptree(x.value);},
        [](const StringLiteral& x) -> ptree {return ptree(add_quotes(x.value));},
        [](const Var& x) -> ptree {return ptree(x.name);},
        [](const ArgRef& x) -> ptree {return ptree("@" + x.name);},
        [](const Placeholder&) -> ptree {return ptree("_");},
        [](const GetState& x) -> ptree {return ptree("get_state", {{"", annotated_ptree_from_typed_model_expr({Ann{ptree("String"), {}, false, {}}, Var{x.state_name}})}});},
        // Converts a typed AST call, preserving argument metadata and absent
        // argument values.
        [](const Call<Ann>& x) -> ptree
        {
            ptree result(x.function);
            for(auto& arg: x.args)
            {
                ptree value;
                if (arg.value)
                {
                    value = annotated_ptree_from_typed_model_expr(arg.value->get());
                    add_arg_metadata(value, arg);
                }
                result.children().push_back({arg.name, value});
            }
            return result;
        },
        // Converts each typed list element to its annotated positional child.
        [](const List<Ann>& x) -> ptree
        {
            ptree result("List");
            for(auto& element: x.elements)
                result.children().push_back({"", annotated_ptree_from_typed_model_expr(element)});
            return result;
        },
        // Converts each typed tuple element after checking tuple arity.
        [](const Tuple<Ann>& x) -> ptree
        {
            if (x.elements.size() < 2)
                throw std::logic_error("cannot convert one-element annotated model tuple to ptree");
            ptree result("Tuple");
            for(auto& element: x.elements)
                result.children().push_back({"", annotated_ptree_from_typed_model_expr(element)});
            return result;
        },
        // Converts a typed let expression to the annotated !let shape.
        [](const Let<Ann>& x) -> ptree
        {
            return ptree("!let", {
                {"decls", annotated_ptree_from_typed_model_decls(x.decls)},
                {"body", annotated_ptree_from_typed_model_expr(x.body.get())}
            });
        },
        // Converts a typed lambda to the annotated function shape.
        [](const Lambda<Ann>& x) -> ptree
        {
            return ptree("function", {
                {"", annotated_ptree_from_typed_model_expr(x.pattern.get())},
                {"", annotated_ptree_from_typed_model_expr(x.body.get())}
            });
        },
        // Emits the codegen-facing sample rule shape, including argument
        // metadata that is not stored on the dedicated AST Sample node.
        [](const Sample<Ann>& x) -> ptree
        {
            auto dist = annotated_ptree_from_typed_model_expr(x.dist.get());
            dist.children().push_back({"is_default_value", ptree(false)});
            return ptree("sample", {{"dist", dist}});
        }
    }, expr.node);

    ptree result({{"value", value}});
    add_ann_to_ptree(result, expr.ann);
    return result;
}

// Converts a legacy annotated !Decls ptree into typed AST declarations.
TypedDecls typed_model_decls_from_annotated_ptree(const ptree& p)
{
    if (not p.has_value<string>() or p.get_value<string>() != "!Decls")
        throw std::logic_error("malformed annotated model ptree for declarations");

    TypedDecls decls;
    for(auto& [name, value]: p.children())
        decls.push_back({name, typed_model_expr_from_annotated_ptree(value)});
    return decls;
}

// Converts typed AST declarations back into the annotated !Decls ptree shape.
ptree annotated_ptree_from_typed_model_decls(const TypedDecls& decls)
{
    ptree result("!Decls");
    for(auto& [name, value]: decls)
        result.children().push_back({name, annotated_ptree_from_typed_model_expr(value)});
    return result;
}

}
