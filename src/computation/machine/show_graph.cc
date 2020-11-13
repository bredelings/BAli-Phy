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

using std::string;
using std::vector;
using std::map;
using std::set;
using std::ofstream;

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
    check_used_regs_in_token(t);
#endif
}

void reg_heap::find_all_regs_in_context_no_check(int, vector<int>& scan, vector<int>& unique) const
{
    for(int i=0;i<scan.size();i++)
    {
	int r = scan[i];

	// This is a hack, but allows drawing graphs when we have edges to missing regs
	if (regs.is_free(r)) continue;

	assert(regs.is_used(r) or regs.is_marked(r));
	if (regs.is_marked(r)) continue;

	regs.set_mark(r);
	unique.push_back(scan[i]);
    }

    for(int i=0;i<unique.size();i++)
    {
	int r = unique[i];
	assert(regs.is_marked(r));

	const reg& R = regs.access(r);
	for(int j:R.C.Env)
	{
	    if (regs.is_used(j) and not regs.is_marked(j))
	    {
		regs.set_mark(j);
		unique.push_back(j);
	    }
	}

	// We can get dependencies on used regs that are not in the environment if we
	// have merged steps.
	for(int j: used_regs_for_reg(r))
	{
	    if (regs.is_used(j) and not regs.is_marked(j))
	    {
		regs.set_mark(j);
		unique.push_back(j);
	    }
	}

	// We can get dependencies on forced regs since we no merge operations that only force things.
	for(int j: forced_regs_for_reg(r))
	{
	    if (regs.is_used(j) and not regs.is_marked(j))
	    {
		regs.set_mark(j);
		unique.push_back(j);
	    }
	}

	// Count also the references from the call
	if (reg_has_call(r))
	{
	    int called_reg = call_for_reg(r);
	    if (not regs.is_marked(called_reg))
	    {
		regs.set_mark(called_reg);
		unique.push_back(called_reg);
	    }
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
    for(const auto& id: ids)
    {
	int R = id.second;
	string name = id.first;
	names[R] = expression_ref(new var(name) );
    }
    discover_graph_vars(C, R, names, ids);

    return launchbury_unnormalize(names[R]);
}

map<int,string> get_register_names(const map<string, int>& ids)
{
    map<int,string> ids2;
    for(const auto& i:ids)
	ids2[i.second] = i.first;
    return ids2;
}

set<string> get_names(const map<string, int>& ids)
{
    set<string> names;
    for(const auto& i:ids)
	names.insert(i.first);
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


void dot_graph_for_token(const reg_heap& C, int t)
{
    string filename = "token" + convertToString(t)+".dot";
    std::ofstream file(filename);
    dot_graph_for_token(C, t, file);
    file.close();
}

/* TODO - to make graph more readable:

   1. Handle indirection nodes, somehow.
   (a) First, check WHY we are getting indirection nodes.
   (b) Then Consider eliminating them somehow during garbage collection.

   2. Allow reduction value (call value) on the same level as redex.

*/

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o)
{
    const auto& ids = C.get_identifiers();

    map<int,string> reg_names = get_register_names(ids);

    map<string,string> simplify = get_simplified_names(get_names(ids));

    map<int,string> constants = get_constants(C, t);

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

	bool print_record = false;
	if (F.head().type() == operation_type or F.head().type() == constructor_type)
	{
	    if (not is_case(F) and not F.head().is_a<Apply>())
	    {
		print_record = true;
		o<<"shape = plain, ";
	    }
	}

	// node label = R/name: expression
	string label = convertToString(R);
	if (reg_names.count(R))
	    label += "/" + reg_names[R];
	label += ":";

	vector<int> targets;
	if (print_record)
	{
	    label = "<table border='0' cellborder='1' cellspacing='0'><tr><td>"+escape(label) + "</td>";

	    label += "<td>"+escape(F.head().print())+"</td>";
	    if (F.is_expression())
		for(const expression_ref& E: F.sub())
		{
		    int index = E.as_index_var();
		    int R2 = C[R].lookup_in_env( index );
		    targets.push_back(R2);
	  
		    string reg_name = "<" + convertToString(R2) + ">";
		    if (constants.count(R2))
			reg_name = constants[R2] + " " + reg_name;
		    else if (reg_names.count(R2))
		    {
			reg_name = reg_names[R2];
			auto loc = simplify.find(reg_name);
			if (loc != simplify.end())
			    reg_name = loc->second;
		    }

		    label += "<td port=\"r" +convertToString(R2)+"\">" + escape(reg_name) + "</td>";
		}
	    label += "</tr></table>";
	}
	else if (F.type() == index_var_type)
	{
	    int R2 = C[R].reg_for_index_var();

	    string reg_name = "<" + convertToString(R2) + ">";
	    if (reg_names.count(R2))
	    {
		reg_name = reg_names[R2];
		auto loc = simplify.find(reg_name);
		if (loc != simplify.end())
		    reg_name = "<" + loc->second + ">";
	    }
	    else if (constants.count(R2))
		reg_name = constants[R2] + " " + reg_name;
	    label += reg_name;
	
	    //      expression_ref E = unlet(untranslate_vars(deindexify(trim_unnormalize(C[R])), reg_names));
	    //      E = map_symbol_names(E, simplify);
	    //      label += E.print();
	    label = escape(wrap(label,40));
	}
	else
	{
	    expression_ref E = unlet(untranslate_vars(untranslate_vars(deindexify(trim_unnormalize(C[R])), reg_names),constants));

	    E = map_symbol_names(E, simplify);

	    label += E.print();
	    label = escape(wrap(label,40));
	}

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
	else if (C.reg_is_constant_no_force(R))
	    o<<",fillcolor=\"#bbbb77\"";
	o<<"];\n";

	// out-edges
	if (print_record)
	{
	    for(int R2: targets)
	    {
		if (not C.reg_is_used(R2)) continue;

		string name2 = "n" + convertToString(R2);
		bool used = false;
		for(int i: C.used_regs_for_reg(R))
		    if (i == R2) used = true;

		// Don't draw ref edges to things like fmap.
		if (reg_names.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

		// Don't draw ref edges to things like fmap.
		if (constants.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

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
		if (reg_names.count(R2) and not C.reg_is_changeable(R2) and not used) continue;
	
		// Don't draw ref edges to things like fmap.
		if (constants.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

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
	    o<<"t"<<t<<" -> t"<<c<<"\n";
	int n_steps = C.get_token(t).vm_step.delta().size();
	int n_results = C.get_token(t).vm_result.delta().size();
	o<<"t"<<t<<" [label=\"T"<<t<<" steps="<<n_steps<<" results = "<<n_results<<"\"]\n";
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

