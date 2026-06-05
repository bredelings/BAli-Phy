#include <fstream>
#include <unordered_set>
#include "graph_register.H"
#include "computation/operations.H"
#include "computation/preprocess.H"
#include "computation/core/subst.H"
#include "computation/expression/modifiable.H"
#include "computation/context.H"
#include "haskell/ids.H"
#include "computation/machine/gcobject.H"
#include "immer/set.hpp"

typedef Box<immer::set<int>> IntSet;

using std::string;
using std::vector;
using std::map;
using std::set;
using std::ofstream;

map<int,string> get_register_names(const map<string, int>& ids, bool allow_compiler_vars);
vector<int> record_targets(const closure& C);

Core::Exp<> map_symbol_names(const Core::Exp<>& E, const std::map<string,string>& simplify)
{
    if (auto V = E.to_var())
    {
        auto loc = simplify.find(V->name);
        if (loc != simplify.end())
        {
            auto V2 = *V;
            V2.name = loc->second;
            return V2;
        }

        return E;
    }
    else if (auto L = E.to_lambda())
        return Core::Lambda<>{L->x, map_symbol_names(L->body, simplify)};
    else if (auto A = E.to_apply())
        return Core::Apply<>{map_symbol_names(A->head, simplify), map_symbol_names(A->arg, simplify)};
    else if (auto L = E.to_let())
    {
        auto decls = L->decls;
        for(auto& [_, body]: decls)
            body = map_symbol_names(body, simplify);

        return Core::Let<>{decls, map_symbol_names(L->body, simplify)};
    }
    else if (auto C = E.to_case())
    {
        auto alts = C->alts;
        for(auto& alt: alts)
            alt.body = map_symbol_names(alt.body, simplify);

        return Core::Case<>{map_symbol_names(C->object, simplify), alts};
    }
    else if (auto C = E.to_conApp())
    {
        auto C2 = *C;
        for(auto& arg: C2.args)
            arg = map_symbol_names(arg, simplify);
        return C2;
    }
    else if (auto B = E.to_builtinOp())
    {
        auto B2 = *B;
        for(auto& arg: B2.args)
            arg = map_symbol_names(arg, simplify);
        return B2;
    }

    return E;
}

bool pattern_binds(const Core::Pattern<>& pattern, const Core::Var<>& x)
{
    for(const auto& arg: pattern.args)
        if (arg == x)
            return true;

    return false;
}

int n_free_occurrences(const Core::Exp<>& E, const Core::Var<>& x)
{
    if (auto V = E.to_var())
        return (*V == x);
    else if (auto L = E.to_lambda())
        return (L->x == x) ? 0 : n_free_occurrences(L->body, x);
    else if (auto A = E.to_apply())
        return n_free_occurrences(A->head, x) + n_free_occurrences(A->arg, x);
    else if (auto L = E.to_let())
    {
        for(const auto& [y, _]: L->decls)
            if (y == x)
                return 0;

        int count = n_free_occurrences(L->body, x);
        for(const auto& [_, body]: L->decls)
            count += n_free_occurrences(body, x);

        return count;
    }
    else if (auto C = E.to_case())
    {
        int count = n_free_occurrences(C->object, x);
        for(const auto& alt: C->alts)
            if (not pattern_binds(alt.pat, x))
                count += n_free_occurrences(alt.body, x);

        return count;
    }
    else if (auto C = E.to_conApp())
    {
        int count = 0;
        for(const auto& arg: C->args)
            count += n_free_occurrences(arg, x);

        return count;
    }
    else if (auto B = E.to_builtinOp())
    {
        int count = 0;
        for(const auto& arg: B->args)
            count += n_free_occurrences(arg, x);

        return count;
    }

    return 0;
}

Core::Exp<> subst_var(const Core::Exp<>& E, const Core::Var<>& x, const Core::Exp<>& replacement)
{
    Core::subst_t<> subst;
    subst = subst.insert({x, replacement});
    return Core::subst(subst, E);
}

Core::Exp<> unlet(const Core::Exp<>& E)
{
    if (auto L = E.to_lambda())
        return Core::Lambda<>{L->x, unlet(L->body)};
    else if (auto C = E.to_case())
    {
        auto alts = C->alts;
        for(auto& alt: alts)
            alt.body = unlet(alt.body);

        return Core::Case<>{unlet(C->object), alts};
    }
    else if (auto L = E.to_let())
    {
        auto L2 = *L;
        L2.body = unlet(L2.body);
        for(auto& [_, body]: L2.decls)
            body = unlet(body);

        bool changed = true;
        while(changed)
        {
            changed = false;
            for(int i = L2.decls.size() - 1; i >= 0; i--)
            {
                const auto x = L2.decls[i].x;
                const auto rhs = L2.decls[i].body;

                if (rhs.to_case()) continue;
                if (n_free_occurrences(rhs, x)) continue;

                int count = n_free_occurrences(L2.body, x);
                for(const auto& [_, body]: L2.decls)
                    count += n_free_occurrences(body, x);

                if (count != 1) continue;

                changed = true;
                L2.decls.erase(L2.decls.begin() + i);

                for(auto& [_, body]: L2.decls)
                    body = subst_var(body, x, rhs);
                L2.body = subst_var(L2.body, x, rhs);
            }
        }

        if (L2.decls.empty())
            return L2.body;
        else
            return L2;
    }
    else if (auto A = E.to_apply())
        return Core::Apply<>{unlet(A->head), unlet(A->arg)};
    else if (auto C = E.to_conApp())
    {
        auto C2 = *C;
        for(auto& arg: C2.args)
            arg = unlet(arg);
        return C2;
    }
    else if (auto B = E.to_builtinOp())
    {
        auto B2 = *B;
        for(auto& arg: B2.args)
            arg = unlet(arg);
        return B2;
    }

    return E;
}

std::map<string,Core::Exp<>> diagnostic_reg_replacements(const map<int,Core::Exp<>>& replace)
{
    std::map<string,Core::Exp<>> replacements;
    for(const auto& [r, E]: replace)
    {
        replacements["<" + convertToString(r) + ">"] = E;
        replacements["[" + convertToString(r) + "]"] = E;
    }

    return replacements;
}

Core::Exp<> subst_diagnostic_vars(const Core::Exp<>& E, const std::map<string,Core::Exp<>>& replacements)
{
    Core::subst_t<> subst;
    for(const auto& [name, replacement]: replacements)
        subst = subst.insert({Core::Var<>(name), replacement});

    return Core::subst(subst, E);
}

Core::Exp<> untranslate_vars(const Core::Exp<>& E, const map<int,string>& ids)
{
    map<int,Core::Exp<>> replace;
    for(const auto& [r, name]: ids)
        replace[r] = Core::Var<>(name);

    return subst_diagnostic_vars(E, diagnostic_reg_replacements(replace));
}

Core::Exp<> untranslate_vars(const Core::Exp<>& E, const map<string, int>& ids)
{
    return untranslate_vars(E, get_register_names(ids, true));
}

vector<int> reg_heap::find_all_regs_in_context_no_check(int t, bool keep_identifiers) const
{
    vector<int> unique;
    find_all_regs_in_context_no_check(t, keep_identifiers, unique);
    return unique;
}

vector<int> reg_heap::find_all_regs_in_context(int t, bool keep_identifiers) const
{
    vector<int> unique;
    find_all_regs_in_context(t, keep_identifiers, unique);
    return unique;
}

vector<int> reg_heap::find_all_used_regs_in_context(int t, bool keep_identifiers) const
{
    vector<int> unique;
    find_all_used_regs_in_context(t, keep_identifiers, unique);
    return unique;
}

void reg_heap::find_all_regs_in_context_no_check(int t, bool keep_identifiers, vector<int>& unique) const
{
    vector<int>& scan = get_scratch_list();

    get_roots(scan, keep_identifiers);

    find_all_regs_in_context_no_check(t,scan,unique);
}

void reg_heap::find_all_used_regs_in_context(int t, bool keep_identifiers, vector<int>& unique) const
{
    vector<int>& scan = get_scratch_list();

    get_roots(scan, keep_identifiers);

    find_all_regs_in_context_no_check(t,scan,unique);

#ifndef NDEBUG
    if (t >= 0) check_used_regs_in_token(t);
#endif
}

void reg_heap::find_all_regs_in_context_no_check(int, vector<int>& scan, vector<int>& unique) const
{
    auto visit_reg = [&](int r) {
        if (regs.is_used(r) and not regs.is_marked(r))
        {
            regs.set_mark(r);
            unique.push_back(r);
        }
    };
    
    for(int r: scan)
        visit_reg(r);

    vector<int> tmp;
    for(int i=0;i<unique.size();i++)
    {
	int r = unique[i];
	assert(regs.is_marked(r));

	const reg& R = regs.access(r);
        if (auto obj = R.C.get_code().to<Runtime::ObjectValue>(); obj and is_gcable_type(obj->value->type()))
        {
            auto gco = convert<GCObject>(obj->value);
            gco->get_regs(tmp);
            for(int j: tmp)
                visit_reg(j);
        }

	for(int j:R.C.Env)
            visit_reg(j);

	for(int j: record_targets(R.C))
            visit_reg(j);

	// We can get dependencies on used regs that are not in the environment if we
	// have merged steps.
	for(int j: used_regs_for_reg(r))
            visit_reg(j);

	// We can get dependencies on forced regs since we no merge operations that only force things.
	for(int j: forced_regs_for_reg(r))
            visit_reg(j);

	// Count also the references from the call
	if (reg_has_call(r))
	{
	    int called_reg = call_for_reg(r);
            visit_reg(called_reg);
	}
    }

#ifndef NDEBUG
    std::unordered_set<int> unique_set;
    for(int r: unique)
    {
        assert(not unique_set.count(r));
        unique_set.insert(r);
    }
#endif

    for(int i=0;i<unique.size();i++)
	regs.unmark(unique[i]);

    release_scratch_list();
}

// This routine is separate from the *_no_check variant because the
// checks don't hold in all cases.
void reg_heap::find_all_regs_in_context(int t, bool keep_identifiers, vector<int>& unique) const
{
    find_all_regs_in_context_no_check(t, keep_identifiers, unique);

#ifdef DEBUG_MACHINE
    check_used_regs_in_token(t);
#endif
}

string escape(const string& s)
{
    string s2;
    s2.resize(s.size()*5);
    int l=0;
    for(int i=0;i<s.size();i++)
    {
	if (s[i] == '\n')
	{
	    s2[l++] = '<';
	    s2[l++] = 'b';
	    s2[l++] = 'r';
	    s2[l++] = '/';
	    s2[l++] = '>';
	}
	else if (s[i] == '<')
	{
	    s2[l++] = '&';
	    s2[l++] = 'l';
	    s2[l++] = 't';
	    s2[l++] = ';';
	}
	else if (s[i] == '>')
	{
	    s2[l++] = '&';
	    s2[l++] = 'g';
	    s2[l++] = 't';
	    s2[l++] = ';';
	}
	else if (s[i] == '&')
	{
	    s2[l++] = '&';
	    s2[l++] = 'a';
	    s2[l++] = 'm';
	    s2[l++] = 'p';
	    s2[l++] = ';';
	}
	else if (s[i] == '"')
	{
	    s2[l++] = '&';
	    s2[l++] = 'q';
	    s2[l++] = 'u';
	    s2[l++] = 'o';
	    s2[l++] = 't';
	    s2[l++] = ';';
	}
	else
	    s2[l++] = s[i];
    }
    s2.resize(l);
    return s2;
}

string wrap(const string& s, int w)
{
    string s2 = s;
    string result;
    while (s2.size())
    {
	int pos = -1;
	if (s2.size() > w)
	    pos = s2.find(' ',w);

	if (result.size())
	    result += "\n";

	if (pos == -1)
	{
	    result += s2;
	    s2 = "";
	}
	else
	{
	    result += s2.substr(0,pos);
	    s2 = s2.substr(pos+1);
	}
    }
    return result;
}

map<int,string> get_register_names(const map<string, int>& ids, bool allow_compiler_vars=true)
{
    map<int,string> ids2;
    for(const auto& [name, r]:ids)
    {
        if (not allow_compiler_vars)
        {
            auto uname = get_unqualified_name(name);
            if (uname.size() > 1 and (uname[0] == '$' or uname[0] == '#'))
                continue;
        }
	ids2[r] = name;
    }
    return ids2;
}

set<string> get_names(const map<string, int>& ids)
{
    set<string> names;
    for(const auto& [name,_]:ids)
	names.insert( name );
    return names;
}

map<int,string> get_constants(const reg_heap& C, int t)
{
    map<int,string> reg_names = get_register_names(C.get_identifiers());

    map<int,string> constants;

    vector<int> regs = C.find_all_used_regs_in_context(t,true);

    // Record some regs as being constants worthy of substituting into regs that reference them.
    for(int R: regs)
    {
	if (reg_names.count(R))
	{
	    auto rname = reg_names.at(R);
	    rname = get_unqualified_name(rname);
	    if (rname.size() and rname[0] != '#') continue;
	}

	if (C[R].is_reg_ref()) continue;

        const auto& code = C[R].get_code();
	if (is_modifiable(code)) continue;

	if (code.is_atomic_value())
	{
	    string name = code.print();
	    if (code.is_double())
	    {
		if (name.find('.') != string::npos)
		{
		    int len = name.find_last_not_of('0');
		    if (name[len] == '.') len++;
		    name = name.substr(0,len+1);
		}
	    }
	    if (name.size() < 20)
		constants[R] = name;
	}
    }
    return constants;
}

Core::Exp<> compact_graph_expression(const reg_heap& C, int R, const map<string, int>& ids)
{
    return unlet(untranslate_vars(runtime_deindexify(C[R]), ids));
}

void write_dot_graph(const reg_heap& C)
{
    int t = C.get_root_token();
    string filename = "token" + convertToString(t)+".dot";
    std::ofstream file(filename);
    write_dot_graph(C,file);
    file.close();
}

/* TODO - to make graph more readable:

   1. Handle indirection nodes, somehow.
   (a) First, check WHY we are getting indirection nodes.
   (b) Then Consider eliminating them somehow during garbage collection.

   2. Allow reduction value (call value) on the same level as redex.

*/

bool print_as_record(const Runtime::Exp& E)
{
    if (E.to<IntMap>())
        return true;

    if (auto app = E.to<Runtime::App>())
        return std::holds_alternative<Runtime::OperationApp>(app->head) or
               std::holds_alternative<Runtime::ConstructorApp>(app->head);

    return false;
}

bool print_as_record(const closure& c)
{
    return print_as_record(c.get_code());
}

vector<int> record_targets(const closure& C)
{
    vector<int> targets;

    if (auto app = C.get_code().to<Runtime::App>())
    {
        for(const auto& arg: app->args)
        {
            if (auto R2 = closure(arg, C.Env).reg_for_ref_maybe())
                targets.push_back(*R2);
            else if (arg.to<Runtime::IndexVar>())
                std::clog<<"closure has bad index in "<<C.get_code()<<std::endl;
        }
    }
    else if (auto im = C.get_code().to<IntMap>())
    {
        for(auto& [_, R]: *im)
            targets.push_back(R);
    }

    return targets;
}

bool contains(const string& s1, const string& s2)
{
    for(int i=0;i+s2.size()<s1.size();i++)
	if (s1.substr(i,s2.size()) == s2)
	    return true;

    return false;
}

string reg_name(int R, const map<int,Core::Exp<>>& replace)
{
    string name = "<" + convertToString(R) + ">";
    // We could add <R2> after the name for non-var objects.
    if (replace.count(R))
    {
	auto E = replace.at(R);
	if (auto V = E.to_var())
	    name = V->name;
	else
	    name = E.print() + " " + name;;
    }
    return name;
}

string direct_ref_name(int R)
{
    return "<" + convertToString(R) + ">";
}

string env_ref_name(int R)
{
    return "[" + convertToString(R) + "]";
}

enum class GraphRefKind { direct, env };

struct GraphRef
{
    int reg;
    GraphRefKind kind;
};

std::optional<GraphRef> graph_ref_for_arg(const Runtime::Exp& E, const closure& CR)
{
    if (auto index_var = E.to<Runtime::IndexVar>())
    {
        if (index_var->index >= CR.Env.size())
            return {};

        return GraphRef{CR.lookup_in_env(index_var->index), GraphRefKind::env};
    }
    else if (auto reg_ref = E.to<Runtime::RegRef>())
        return GraphRef{reg_ref->target, GraphRefKind::direct};
    else
        return {};
}

string render_register_graph_table_arg_cell(const Runtime::Exp& E, const closure& CR, const map<int,Core::Exp<>>& replace)
{
    closure arg(E, CR.Env);
    if (auto ref = graph_ref_for_arg(E, CR))
    {
        auto name = ref->kind == GraphRefKind::direct ? direct_ref_name(ref->reg) : env_ref_name(ref->reg);
        return "<td port=\"r" + convertToString(ref->reg) + "\">" + escape(name) + "</td>";
    }

    auto E2 = subst_diagnostic_vars(runtime_deindexify(arg), diagnostic_reg_replacements(replace));
    return "<td>" + escape(E2.print()) + "</td>";
}

map<int,Core::Exp<>> make_factor_graph_replacements(const map<int,string>& reg_names, const map<int,string>& constants)
{
    map<int,Core::Exp<>> replace;
    for(const auto& [r, name]: reg_names)
        replace[r] = Core::Var<>(name);
    for(const auto& [r, name]: constants)
        replace[r] = Core::Var<>(name);

    return replace;
}

string render_factor_graph_table_arg_cell(const Runtime::Exp& E, const closure& CR,
                                          const map<int,string>& reg_names,
                                          const map<int,string>& constants,
                                          const map<string,string>& simplify)
{
    closure arg(E, CR.Env);
    if (auto ref = graph_ref_for_arg(E, CR))
    {
        auto name = ref->kind == GraphRefKind::direct ? direct_ref_name(ref->reg) : env_ref_name(ref->reg);
        return "<td port=\"r" + convertToString(ref->reg) + "\">" + escape(name) + "</td>";
    }

    auto E2 = runtime_deindexify(arg);
    E2 = subst_diagnostic_vars(E2, diagnostic_reg_replacements(make_factor_graph_replacements(reg_names, constants)));
    E2 = map_symbol_names(E2, simplify);

    return "<td>" + escape(E2.print()) + "</td>";
}

closure follow_reg_ref(const reg_heap& M, closure C)
{
    for(int& r: C.Env)
	r = M.follow_reg_ref(r);
    return C;
}

string label_for_reg(int R, const reg_heap& C, const map<int,Core::Exp<>>& replace, bool skip_ref = false)
{
    auto CR = C[R];
    if (skip_ref)
	CR = follow_reg_ref(C, CR);

    // node label = R/name: expression
    string label = convertToString(R);
    if (replace.count(R))
    {
	auto E = replace.at(R);
	if (auto V = E.to_var())
	    label += "/" + V->name;
    }
    label += ":";

    if (print_as_record(CR.get_code()))
    {
        label = "<table border='0' cellborder='1' cellspacing='0'><tr><td>"+escape(label) + "</td>";

        if (CR.get_code().to<IntMap>())
            label += "<td>IntMap</td>";
        else if (auto app = CR.get_code().to<Runtime::App>())
            label += "<td>"+escape(Runtime::print(app->head))+"</td>";
        else
            label += "<td>"+escape(CR.get_code().print())+"</td>";
        if (auto app = CR.get_code().to<Runtime::App>())
	{
            for(const auto& E: app->args)
                label += render_register_graph_table_arg_cell(E, CR, replace);
	}
        else if (auto im = CR.get_code().to<IntMap>())
        {
            for(auto& [key, R2]: *im)
                label += "<td port=\"r" +convertToString(R2)+"\">" + std::to_string(key) + "&rarr;" + escape(direct_ref_name(R2)) + "</td>";
        }
        label += "</tr></table>";
    }
    else if (CR.is_reg_ref())
    {
        int R2 = C[R].reg_for_ref();

        label += reg_name(R2, replace);

        label = escape(wrap(label,40));
    }
    else if (auto is = CR.get_code().to<IntSet>())
    {
	std::ostringstream o;
	o<<"IntSet{";
	bool first = true;
	for(auto& x: *is)
	{
	    if (first)
		first = false;
	    else
		o<<",";

	    o<<x;
	}
	o<<"}";
	label += o.str();
    }
    else
    {
        auto E = unlet(subst_diagnostic_vars(runtime_deindexify(C[R]), diagnostic_reg_replacements(replace)));

        label += E.print();
        label = escape(wrap(label,40));
    }
    return label;
}

string label_for_reg2(int R, const reg_heap& C, const map<int,string>& reg_names,
                      const map<int,string>& constants, const map<string,string>& simplify)
{
    auto CR = C[R];
    for(int& r: CR.Env)
        r = C.follow_reg_ref(r);

    // node label = R/name: expression
    string label;
    if (reg_names.count(R))
        label = "/" + reg_names.at(R) + ":";

    if (print_as_record(CR.get_code()))
    {
        label = "<table border='0' cellborder='1' cellspacing='0'><tr>";

        string head_name = CR.get_code().print();
        if (auto app = CR.get_code().to<Runtime::App>())
            head_name = Runtime::print(app->head);

        // Chop for module prefix in module:builtin
        int where = head_name.find(':');
        if (where != string::npos and where+1 < head_name.size())
            head_name = head_name.substr(where+1);

        label += "<td>"+escape(head_name)+"</td>";
        if (auto app = CR.get_code().to<Runtime::App>())
	{
            for(const auto& E: app->args)
                label += render_factor_graph_table_arg_cell(E, CR, reg_names, constants, simplify);
	}
        label += "</tr></table>";
    }
    else if (CR.is_reg_ref())
    {
        int R2 = C[R].reg_for_ref();

        string reg_name = " ";
        if (reg_names.count(R2))
        {
            reg_name = reg_names.at(R2);
            auto loc = simplify.find(reg_name);
            if (loc != simplify.end())
                reg_name = "<" + loc->second + ">";
        }
        else if (constants.count(R2))
            reg_name = constants.at(R2);
        label += reg_name;

        label = escape(wrap(label,40));
    }
    else if (is_modifiable(CR.get_code()))
        label="mod";
    else
    {
        auto replace = make_factor_graph_replacements(reg_names, constants);
        auto E = unlet(subst_diagnostic_vars(runtime_deindexify(CR), diagnostic_reg_replacements(replace)));
        E = map_symbol_names(E, simplify);

        label += E.print();
        label = escape(wrap(label,40));
    }
    return label;
}

map<int,Core::Exp<>> get_names_for_regs(const reg_heap& M)
{
    // Get mapping from long names to simplified names
    map<string,string> simplify = get_simplified_names(get_names(M.get_identifiers()));

    // Get mapping from regs to vars with simplified names.
    map<int,Core::Var<>> reg_to_var;
    for(auto& [name, reg]: M.get_identifiers())
    {
        auto sname = name;
	if (simplify.contains(name))
	    sname = simplify.at(name);
	reg_to_var.insert({reg, Core::Var<>(sname)});
    }

    map<int,Core::Exp<>> reg_to_expression;

    int t = M.get_root_token();

    vector<int> regs = M.find_all_used_regs_in_context(t,false);

    for(int r: regs)
    {
	if (auto E = M.closure_at(r).get_code(); E.is_atomic_value() and not E.to<GCObject>() and E.print().size() < 25)
	{
	    reg_to_expression.insert({r,runtime_deindexify(E)});
	}
	else if (reg_to_var.contains(r))
	{
	    reg_to_expression.insert({r,reg_to_var.at(r)});
	}
    }
    return reg_to_expression;
}

void write_dot_graph(const reg_heap& C, std::ostream& o)
{
    int t = C.get_root_token();

    map<int,Core::Exp<>> replace = get_names_for_regs(C);

    vector<int> regs = C.find_all_used_regs_in_context(t,false);
    std::unordered_set<int> regs_set;
#ifndef NDEBUG
    for(int r: regs)
        regs_set.insert(r);
#endif

    o<<"digraph \"token"<<t<<"\" {\n";
    o<<"graph [ranksep=0.25, fontname=Arial,  nodesep=0.25, ranksep=0.5];\n";
    o<<"node [fontname=Arial, style=filled, height=0, width=0, shape=box];\n";
    o<<"edge [style=\"setlinewidth(2)\"];\n";
    for(int R:regs)
    {
	string name = "n" + convertToString(R);
	// node name
	o<<name<<" ";
	o<<"[";

	bool print_record = print_as_record(C[R].get_code());
        if (print_record)
            o<<"shape = plain, ";

        // node label = R/name: expression
	string label = label_for_reg(R, C, replace);
	o<<"label = <"<<label<<">";
//	if (this is a gc root) // maybe call get_roots, and then make a set<int> of all the roots?
//	    o<<",style=\"dashed,filled\",color=orange";
	if (C.reg_is_changeable(R))
	    o<<",style=\"dashed,filled\",color=red";

	if (C.reg_is_changeable(R) and C.has_result1(R))
	    o<<",fillcolor=\"#007700\",fontcolor=white";
	else if (C.reg_is_changeable(R))
	    o<<",fillcolor=\"#770000\",fontcolor=white";
	else if (C[R].is_reg_ref())
	    o<<",fillcolor=\"#77bbbb\"";
	else if (C.reg_is_constant(R))
	    o<<",fillcolor=\"#bbbb77\"";
	o<<"];\n";

	// out-edges
	if (print_record)
	{
            vector<int> targets = record_targets(C[R]);

	    for(int R2: targets)
	    {
		if (not C.reg_is_used(R2)) continue;

		string name2 = "n" + convertToString(R2);
		bool used = false;
		for(int i: C.used_regs_for_reg(R))
		    if (i == R2) used = true;

		// Don't draw ref edges to things like fmap.
		if (replace.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

		if (not used)
		    o<<name<<":r"<<R2<<" -> "<<name2<<";\n";
		else
		    o<<name<<":r"<<R2<<" -> "<<name2<<" [color=\"#007777\"];\n";
	    }
	}
	else
	{
	    for(int R2: C[R].Env)
	    {
		if (not C.reg_is_used(R2)) continue;

		string name2 = "n" + convertToString(R2);
		bool used = false;
		for(int i: C.used_regs_for_reg(R))
		    if (i == R2) used = true;

		// Don't draw ref edges to things like fmap.
		if (replace.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

		if (not used)
		    o<<name<<" -> "<<name2<<";\n";
		else
		    o<<name<<" -> "<<name2<<" [color=\"#007777\"];\n";
	    }
	}

	// call-edges
	// FIXME:Drawing - how can allow these to go to the right, but not above, if no ref edges?
	if (C.reg_has_call(R))
	{
	    int R2 = C.call_for_reg(R);
	    assert(regs_set.count(R2));

	    bool created_call = false;
	    for(int r: C.step_for_reg(R).created_regs)
		if (r == R2)
		    created_call = true;

	    string name2 = "n" + convertToString(R2);
	    o<<name<<":e -> "<<name2<<":w ";
	    o<<"[color=\"#007700\"]";
	    o<<"\n";
	    if (created_call)
		o<<"{rank = same; "<<name<<"; "<<name2<<";}"<<std::endl;
	}

	// used_regs
	for(int R2: C.used_regs_for_reg(R))
	{
	    bool is_ref_edge_also = false;
	    for(int R3: C[R].Env)
		if (R2 == R3)
		    is_ref_edge_also = true;

	    if (is_ref_edge_also) continue;

	    string name2 = "n" + convertToString(R2);
	    o<<name<<" -> "<<name2<<" ";
	    o<<"[";
	    o<<"color=\"#007777\"";
	    o<<",style=dashed";
	    o<<"];\n";
	}

	// forced_regs
	for(int R2: C.forced_regs_for_reg(R))
	{
	    bool is_ref_edge_also = false;
	    for(int R3: C[R].Env)
		if (R2 == R3)
		    is_ref_edge_also = true;

	    if (is_ref_edge_also) continue;

	    string name2 = "n" + convertToString(R2);
	    o<<name<<" -> "<<name2<<" ";
	    o<<"[";
	    o<<"color=\"#770077\"";
	    o<<",style=dashed";
	    o<<"];\n";
	}

    }
    o<<"}"<<std::endl;
}

struct dist_group
{
    string dist_type;
    std::map<std::string,int> inputs;
};

bool operator==(const dist_group& d1, const dist_group& d2)
{
    return d1.dist_type == d2.dist_type and d1.inputs == d2.inputs;
}

bool operator<(const dist_group& d1, const dist_group& d2)
{
    if (d1.dist_type < d2.dist_type) return true;
    if (d1.dist_type > d2.dist_type) return false;

    return (d1.inputs < d2.inputs);
}

void context_ref::write_factor_graph(std::ostream& o) const
{
    evaluate_program();
    collect_garbage();

    auto& M = *memory();

    int t = M.token_for_context(context_index);

    const auto& ids = M.get_identifiers();

    map<int,string> reg_names = get_register_names(ids, false);

    map<string,string> simplify = get_simplified_names(get_names(ids));

    map<int,string> constants = get_constants(M, t);

    vector<int> regs = M.find_all_used_regs_in_context(t,false);

    std::unordered_set<int> regs2_set;
    vector<int> regs2;
    std::map<dist_group,int> distributions;

    o<<"digraph \"token"<<t<<"\" {\n";
    o<<"graph [ranksep=0.25, fontname=Arial,  nodesep=0.25, ranksep=0.5];\n";
    o<<"node [fontname=Arial, style=filled, height=0, width=0, shape=box];\n";
    o<<"edge [style=\"setlinewidth(2)\"];\n";

    // We need to group sampling events with the same:
    // (a) name
    // (b) map: string -> r

    for(auto& [s,name]: M.dist_type)
    {
        // Characterize the distribution here.
        dist_group D;
        D.dist_type = name;
        if (auto in_edges = in_edges_to_dist(s))
            for(auto& arg_name: in_edges->arg_names())
            {
                int r = *in_edges->get(arg_name);
                int r2 = M.follow_reg_ref(r);
                D.inputs.insert({arg_name,r2});
                regs2_set.insert(r2);
            }

        // Characterize the out-edge here.
        int r_out = *out_edges_from_dist(s);
        r_out = M.follow_reg_ref(r_out);

        // Get a node number for the distribution.
        int S = s;
        auto it = distributions.find(D);
        if (it == distributions.end())
            distributions[D] = S;
        else
            S = it->second;

        // Draw the out-edge
        o<<"s"<<S<<" -> r"<<r_out<<"\n";

        // Is `s` an observation or not?
        o<<"r"<<r_out<<"   [color=\"#cc9999\"]\n";
        regs2_set.insert(r_out);
    }

    // Draw edges for the distributions.
    for(auto& [D,s]: distributions)
    {
        o<<"s"<<s<<"   [label=\" \",xlabel=\""<<D.dist_type<<"\",color=\"black\"]\n";
        for(auto& [arg_name, r]: D.inputs)
            o<<"r"<<r<<" -> s"<<s<<"  [label=\""<<arg_name<<"\"]\n";
    }

    std::unordered_set<int> direct_regs = regs2_set;
    for(auto r: regs2_set)
        regs2.push_back(r);
    
    for(int i=0;i<regs2.size();i++)
    {
        int r = regs2[i];
        if (is_modifiable(M[r].get_code())) continue;
        if (print_as_record(M[r].get_code()))
            for(auto r2: record_targets(M[r]))
            {
                r2 = M.follow_reg_ref(r2);
                if (not regs2_set.count(r2))
                {
                    regs2_set.insert(r2);
                    regs2.push_back(r2);
                }
            }

        for(auto r2: M[r].Env)
        {
            r2 = M.follow_reg_ref(r2);
            if (not regs2_set.count(r2))
            {
                regs2_set.insert(r2);
                regs2.push_back(r2);
            }
        }
    }

    for(int r: regs2)
    {
        // This isn't quite good enough -- are there any in-edges to this node?
        if (not M.reg_is_changeable(r) and not direct_regs.count(r))
        {
            if (reg_names.count(r) or constants.count(r)) continue;
        }

        // node label = R/name: expression
	string name = "r" + convertToString(r);

        bool print_record = print_as_record(M[r].get_code());
        if (print_record)
            o<<"r"<<r<<"  [label=<"<<label_for_reg2(r,M,reg_names,constants,simplify)<<">,shape=plain]\n";
        else
            o<<"r"<<r<<"  [label=<"<<label_for_reg2(r,M,reg_names,constants,simplify)<<">]\n";

        // out-edges
	if (print_record)
	{
            vector<int> targets = record_targets(M[r]);
            for(int& r2: targets)
                r2 = M.follow_reg_ref(r2);

	    for(int r2: targets)
	    {
		if (not M.reg_is_used(r2)) continue;

		string name2 = "r" + convertToString(r2);

		// Don't draw ref edges to things like fmap.
		if (reg_names.count(r2) and not M.reg_is_changeable(r2)) continue;

		// Don't draw ref edges to things like fmap.
		if (constants.count(r2) and not M.reg_is_changeable(r2)) continue;

                o<<name2<<":s -> "<<name<<":r"<<r2<<";\n";
	    }
	}
	else
	{
            if (is_modifiable(M[r].get_code())) continue;

	    for(int r2: M[r].Env)
	    {
		if (not M.reg_is_used(r2)) continue;

                r2 = M.follow_reg_ref(r2);
		string name2 = "r" + convertToString(r2);

		// Don't draw ref edges to things like fmap.
		if (reg_names.count(r2) and not M.reg_is_changeable(r2)) continue;
	
		// Don't draw ref edges to things like fmap.
		if (constants.count(r2) and not M.reg_is_changeable(r2)) continue;

                o<<name2<<":s -> "<<name<<";\n";
	    }
	}
    }
    o<<"}"<<std::endl;
}

void context_ref::write_factor_graph() const
{
    int t = memory()->token_for_context(context_index);
    string filename = "factor-" + convertToString(t)+".dot";
    std::ofstream file(filename);
    write_factor_graph(file);
    file.close();
}

void write_token_graph(const reg_heap& C, std::ostream& o)
{
    o<<"digraph \"tokens\" {\n";
    o<<"graph [ranksep=0.25, fontname=Arial,  nodesep=0.25, ranksep=0.5];\n";
    o<<"node [fontname=Arial, style=filled, height=0, width=0, shape=box];\n";
    o<<"edge [style=\"setlinewidth(2)\"];\n";
    for(int t=0;t<C.get_n_tokens();t++)
    {
	if (not C.token_is_used(t)) continue;
	for(int c: C.children_of_token(t))
	    o<<"t"<<t<<" -> t"<<c<<" [label=\""<<C.directed_token_type(c)<<"\"]\n";
	int n_steps = C.get_token(t).vm_step.delta().size();
	int n_results = C.get_token(t).vm_result.delta().size();
	o<<"t"<<t<<" [label=\"t="<<t
	 <<"  age="<<C.get_token(t).creation_time
//	 <<"\\ntype="<<C.directed_token_type(t)
	 <<"\\nutype="<<C.undirected_token_type(t)
	 <<"\\nsteps="<<n_steps<<"  results = "<<n_results<<"\"]\n";
    }
    for(int c=0;c<C.get_n_contexts();c++)
    {
	int t = C.token_for_context(c);
	if (t == -1) continue;
	o<<"t"<<t<<" [color=\"green\"]\n";
	o<<"c"<<c<<" -> t"<<t<<"\n";
	o<<"c"<<c<<" [color=\"purple\"]\n";
    }

    o<<"}"<<std::endl;
}

void write_token_graph(const reg_heap& C)
{
    string filename = "tokens.dot";
    std::ofstream file(filename);
    write_token_graph(C, file);
    file.close();
}
