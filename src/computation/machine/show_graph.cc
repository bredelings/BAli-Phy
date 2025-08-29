#include <fstream>
#include <unordered_set>
#include "graph_register.H"
#include "computation/operations.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"
#include "computation/expression/case.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/expression.H" // for launchbury_unnormalize( )
#include "util/set.H"
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

expression_ref map_symbol_names(const expression_ref& E, const std::map<string,string>& simplify)
{
    if (not E.size())
    {
	if (is_qualified_var(E))
	{
	    auto x = E.as_<var>();
	    auto loc = simplify.find(x.name);
	    if (loc != simplify.end())
		return var(loc->second);
	}
	return E;
    }

    object_ptr<expression> V = E.as_expression().clone();
    for(int i=0;i<E.size();i++)
	V->sub[i] = map_symbol_names(V->sub[i], simplify);
    return V;
}


expression_ref subst_reg_vars(const expression_ref& E, const map<int,expression_ref>& replace)
{
    if (auto rv = E.to<reg_var>())
    {
	if (replace.contains(rv->target))
	    return replace.at(rv->target);
	else
	    return E;
    }
    else if (auto alts = E.to<Core::Alts>())
    {
	auto alts2 = new Core::Alts(*alts);
	for(auto& [pattern,body]: *alts2)
	{
	    pattern = subst_reg_vars(pattern, replace);
	    body = subst_reg_vars(body, replace);
	}
	return expression_ref(alts2);
    }
    else if (auto let = E.to<let_exp>())
    {
	auto let2 = new let_exp(*let);
	let2->body = subst_reg_vars(let2->body, replace);
	for(auto& [x,E2]: let2->binds)
	    E2 = subst_reg_vars(E2, replace);
	return expression_ref(let2);
    }
    else if (E.size() == 0)
	return E;
    else
    {
	auto sub = E.sub();
	for(auto& e: sub)
	    e = subst_reg_vars(e, replace);
	return expression_ref{E.head(), sub};
    }
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
        if (auto& obj = R.C.exp; is_gcable_type(obj.type()))
        {
            auto gco = convert<GCObject>(obj.ptr());
            gco->get_regs(tmp);
            for(int j: tmp)
                visit_reg(j);
        }

	for(int j:R.C.Env)
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

// Fixme!
// Here we have handled neither depths, nor trim.
expression_ref subst_referenced_vars(const expression_ref& E, const closure::Env_t& Env, const map<int, expression_ref>& names)
{
    if (E.size())
    {
	bool different = false;
	object_ptr<expression> E2 ( new expression(E.head()) );
	E2->sub.resize(E.size());
	for(int i=0;i<E.size();i++)
	{
	    E2->sub[i] = subst_referenced_vars(E.sub()[i], Env, names);
	    if (E2->sub[i].ptr() != E.sub()[i].ptr())
		different = true;
	}
	if (different)
	    return object_ptr<const expression>(E2);
	else
	    return E;
    }
    else if (auto alts = E.to<Core::Alts>())
    {
	auto alts2 = new Core::Alts(*alts);
	for(auto& [pattern,body]: *alts2)
	{
	    pattern = subst_referenced_vars(pattern, Env, names);
	    body = subst_referenced_vars(body, Env, names);
	}
	return expression_ref(alts2);
    }
    else if (auto let = E.to<let_exp>())
    {
	auto let2 = new let_exp(*let);
	let2->body = subst_referenced_vars(let2->body, Env, names);
	for(auto& [x,E2]: let2->binds)
	    E2 = subst_referenced_vars(E2, Env, names);
	return expression_ref(let2);
    }
    else if ( E.is_index_var() )
    {
	const auto loc = names.find( lookup_in_env(Env, E.as_index_var()) );
	if (loc == names.end())
	    return E;
	else
	{
	    //      assert(get_free_indices(loc->second).empty());
	    return loc->second;
	}
    }
    // This case handles NULL in addition to atomic objects.
    else
	return E;
}

void discover_graph_vars(const reg_heap& H, int R, map<int,expression_ref>& names, const map<string, int>& id)
{
    const closure& C = H[R];

    // If there are no references, then we are done.
    if (C.Env.empty()) 
    {
	names[R] = C.exp;
	return;
    }

    // If R references R, then terminate the recursion.
    if (names.count(R))
    {
	if (not names[R])
	    names[R] = C.exp;
	return;
    }

    // Add R to the hash in order to avoid infinite loops because of re-entering R
    names[R] = expression_ref();

    // find the names for each referenced var.
    for(int i: C.Env)
	discover_graph_vars(H, i, names, id);

    for(int r: H.forced_regs_for_reg(R))
	discover_graph_vars(H, r, names, id);

    names[R] = subst_referenced_vars(C.exp, C.Env, names);
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

expression_ref untranslate_vars(const expression_ref& E, const map<int,string>& ids)
{
    if (not E.size())
    {
	if (E.is_a<reg_var>())
	{
	    auto loc = ids.find(E.as_<reg_var>().target);
	    if (loc != ids.end())
		return var(loc->second);  // Using var( ) here used to cause a problem if the name is not a legal Haskell identifier.
	    else
		return E;
	}
	else
	    return E;
    }

    object_ptr<expression> V = E.as_expression().clone();
    for(int i=0;i<E.size();i++)
	V->sub[i] = untranslate_vars(V->sub[i], ids);
    return V;
}

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, int>& ids)
{
    return C[R].exp;

    map< int, expression_ref> names;
    for(const auto& [name, R]: ids)
    {
	names[R] = expression_ref(new var(name) );
    }
    discover_graph_vars(C, R, names, ids);

    return launchbury_unnormalize(names[R]);
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

	if (C[R].exp.is_index_var()) continue;

	if (is_modifiable(C[R].exp)) continue;

	if (C[R].exp.size() == 0)
	{
	    string name = C[R].exp.print();
	    if (C[R].exp.is_double())
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

expression_ref untranslate_vars(const expression_ref& E, const map<string, int>& ids)
{
    return untranslate_vars(E, get_register_names(ids));
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

bool print_as_record(const expression_ref& E)
{
    if (E.head().is_a<IntMap>())
        return true;
    else if (E.head().type() == type_constant::operation_type or E.head().type() == type_constant::constructor_type)
    {
        if (not is_case(E) and not E.head().is_a<Apply>())
            return true;
    }
    return false;
}

bool print_as_record(const closure& c)
{
    return print_as_record(c.exp);
}

bool contains(const string& s1, const string& s2)
{
    for(int i=0;i+s2.size()<s1.size();i++)
	if (s1.substr(i,s2.size()) == s2)
	    return true;

    return false;
}

string reg_name(int R, const map<int,expression_ref>& replace)
{
    string name = "<" + convertToString(R) + ">";
    // We could add <R2> after the name for non-var objects.
    if (replace.count(R))
    {
	auto E = replace.at(R);
	if (E.is_a<var>())
	    name = replace.at(R).print();
	else
	    name = replace.at(R).print() + " " + name;;
    }
    return name;
}

closure follow_index_var(const reg_heap& M, closure C)
{
    for(int& r: C.Env)
	r = M.follow_index_var(r);
    return C;
}

string label_for_reg(int R, const reg_heap& C, const map<int,expression_ref>& replace, bool skip_index_var = false)
{
    auto CR = C[R];
    if (skip_index_var)
	CR = follow_index_var(C, CR);

    expression_ref F = CR.exp;
    // node label = R/name: expression
    string label = convertToString(R);
    if (replace.count(R))
    {
	auto E = replace.at(R);
	if (E.is_a<var>())
	    label += "/" + E.print();
    }
    label += ":";

    if (print_as_record(F))
    {
        label = "<table border='0' cellborder='1' cellspacing='0'><tr><td>"+escape(label) + "</td>";

        if (F.is_a<IntMap>())
            label += "<td>IntMap</td>";
        else
            label += "<td>"+escape(F.head().print())+"</td>";
        if (F.is_expression())
	{
            for(const expression_ref& E: F.sub())
            {
                if (E.is_index_var())
                {
                    int index = E.as_index_var();
                    int R2 = -1;
                    if (index < CR.Env.size())
                        R2 = CR.lookup_in_env( index );
                    else
                        std::clog<<"reg "<<R<<" has bad index in "<<CR.print()<<std::endl;
	  
                    label += "<td port=\"r" +convertToString(R2)+"\">" + escape(reg_name(R2,replace)) + "</td>";
                }
                else
                {
                    label += "<td>" + E.print() + "</td>";
                }
            }
	}
        else if (auto im = F.to<IntMap>())
        {
            for(auto& [key, R2]: *im)
                label += "<td port=\"r" +convertToString(R2)+"\">" + std::to_string(key) + "&rarr;" + escape(reg_name(R2,replace)) + "</td>";
        }
        label += "</tr></table>";
    }
    else if (F.type() == type_constant::index_var_type)
    {
        int R2 = C[R].reg_for_index_var();

        label += reg_name(R2, replace);

        label = escape(wrap(label,40));
    }
    else if (auto is = F.head().to<IntSet>())
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
        expression_ref E = unlet(subst_reg_vars(deindexify(trim_unnormalize(C[R])), replace));

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
        r = C.follow_index_var(r);

    expression_ref F = CR.exp;
    // node label = R/name: expression
    string label;
    if (reg_names.count(R))
        label = "/" + reg_names.at(R) + ":";

    if (print_as_record(F))
    {
        label = "<table border='0' cellborder='1' cellspacing='0'><tr>";

        string head_name = F.head().print();

        // Chop for module prefix in module:builtin
        int where = head_name.find(':');
        if (where != string::npos and where+1 < head_name.size())
            head_name = head_name.substr(where+1);

        label += "<td>"+escape(head_name)+"</td>";
        if (F.is_expression())
	{
            for(const expression_ref& E: F.sub())
            {
                if (E.is_index_var())
                {
                    int index = E.as_index_var();
                    int R2 = CR.lookup_in_env( index );

                    string reg_name = " ";
                    if (constants.count(R2))
                        reg_name = constants.at(R2);
                    else if (reg_names.count(R2))
                    {
                        reg_name = reg_names.at(R2);
                        auto loc = simplify.find(reg_name);
                        if (loc != simplify.end())
                            reg_name = loc->second;
                    }

                    label += "<td port=\"r" +convertToString(R2)+"\">" + escape(reg_name) + "</td>";
                }
                else
                {
                    label += "<td>" + E.print() + "</td>";
                }
            }
	}
        label += "</tr></table>";
    }
    else if (F.type() == type_constant::index_var_type)
    {
        int R2 = C[R].reg_for_index_var();

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
	
        //      expression_ref E = unlet(untranslate_vars(deindexify(trim_unnormalize(C[R])), reg_names));
        //      E = map_symbol_names(E, simplify);
        //      label += E.print();
        label = escape(wrap(label,40));
    }
    else if (is_modifiable(F))
        label="mod";
    else
    {
        expression_ref E = unlet(untranslate_vars(untranslate_vars(deindexify(trim_unnormalize(C[R])), reg_names),constants));

        E = map_symbol_names(E, simplify);

        label += E.print();
        label = escape(wrap(label,40));
    }
    return label;
}

map<int,expression_ref> get_names_for_regs(const reg_heap& M)
{
    // Get mapping from long names to simplified names
    map<string,string> simplify = get_simplified_names(get_names(M.get_identifiers()));

    // Get mapping from regs to vars with simplified names.
    map<int,var> reg_to_var;
    for(auto& [name, reg]: M.get_identifiers())
    {
        auto sname = name;
	if (simplify.contains(name))
	    sname = simplify.at(name);
	reg_to_var.insert({reg, var(sname)});
    }

    map<int,expression_ref> reg_to_expression;

    int t = M.get_root_token();

    vector<int> regs = M.find_all_used_regs_in_context(t,false);

    for(int r: regs)
    {
	if (auto E = M.expression_at(r); is_WHNF(E) and E.size() == 0 and not E.is_a<GCObject>() and E.print().size() < 25)
	{
	    reg_to_expression.insert({r,E});
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

    map<int,expression_ref> replace = get_names_for_regs(C);

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

	expression_ref F = C[R].exp;

	bool print_record = print_as_record(F);
        if (print_as_record(F))
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
	else if (C[R].exp.is_index_var())
	    o<<",fillcolor=\"#77bbbb\"";
	else if (C.reg_is_constant(R))
	    o<<",fillcolor=\"#bbbb77\"";
	o<<"];\n";

	// out-edges
	if (print_record)
	{
            vector<int> targets;
            if (F.is_expression())
                for(const expression_ref& E: F.sub())
                {
                    if (E.is_index_var())
                    {
                        int index = E.as_index_var();
                        if (index < C[R].Env.size())
                        {
                            int R2 = C[R].lookup_in_env( index );
                            targets.push_back(R2);
                        }
                        else
                            std::clog<<"reg "<<R<<" has bad index in "<<C[R].print()<<std::endl;
                    }
                }
            else if (auto im = F.to<IntMap>())
            {
                for(auto& [_,R]: *im)
                    targets.push_back(R);
            }

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
                int r2 = M.follow_index_var(r);
                D.inputs.insert({arg_name,r2});
                regs2_set.insert(r2);
            }

        // Characterize the out-edge here.
        int r_out = *out_edges_from_dist(s);
        r_out = M.follow_index_var(r_out);

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
        if (is_modifiable(M[r].exp)) continue;
        for(auto r2: M[r].Env)
        {
            r2 = M.follow_index_var(r2);
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

        expression_ref F = M[r].exp;
        // node label = R/name: expression
	string name = "r" + convertToString(r);

        if (print_as_record(F))
            o<<"r"<<r<<"  [label=<"<<label_for_reg2(r,M,reg_names,constants,simplify)<<">,shape=plain]\n";
        else
            o<<"r"<<r<<"  [label=<"<<label_for_reg2(r,M,reg_names,constants,simplify)<<">]\n";

        // out-edges
	if (print_as_record(F))
	{
            vector<int> targets;
            if (F.is_expression())
                for(const expression_ref& E: F.sub())
                {
                    if (E.is_index_var())
                    {
                        int index = E.as_index_var();
                        int r2 = M[r].lookup_in_env( index );
                        r2 = M.follow_index_var(r2);
                        targets.push_back(r2);
                    }
                }

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
            if (is_modifiable(M[r].exp)) continue;

	    for(int r2: M[r].Env)
	    {
		if (not M.reg_is_used(r2)) continue;

                r2 = M.follow_index_var(r2);
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

