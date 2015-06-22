#include <fstream>
#include "graph_register.H"
#include "operations.H"

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
  for(int R: unique)
    check_used_reg(R);
#endif
}

void reg_heap::find_all_regs_in_context_no_check(int t, vector<int>& scan, vector<int>& unique) const
{
  for(int i=0;i<scan.size();i++)
  {
    int r = scan[i];

    // This is a hack, but allows drawing graphs when we have edges to missing regs
    if (is_free(r)) continue;

    assert(is_used(r) or is_marked(r));
    if (is_marked(r)) continue;

    set_mark(r);
    unique.push_back(scan[i]);
  }

  for(int i=0;i<unique.size();i++)
  {
    int r = unique[i];
    assert(is_marked(r));

    const reg& R = access(r);
    for(int j:R.C.Env)
    {
      if (is_used(j) and not is_marked(j))
      {
	set_mark(j);
	unique.push_back(j);
      }
    }

    if (not has_computation(r)) continue;

    // Count also the references from the call
    if (reg_has_call(r))
    {
      int called_reg = call_for_reg(r);
      if (not is_marked(called_reg))
      {
	set_mark(called_reg);
	unique.push_back(called_reg);
      }
    }
  }

#ifndef NDEBUG
  for(int i=0;i<unique.size();i++)
    for(int j=0;j<i;j++)
      assert(unique[i] != unique[j]);
#endif

  for(int i=0;i<unique.size();i++)
    unmark(unique[i]);

  release_scratch_list();
}

// This routine is separate from the *_no_check variant because the
// checks don't hold in all cases.
void reg_heap::find_all_regs_in_context(int t, bool keep_identifiers, vector<int>& unique) const
{
  find_all_regs_in_context_no_check(t, keep_identifiers, unique);

#ifdef DEBUG_MACHINE
  for(int R: unique)
  {
    assert(reg_is_owned_by(R,t));
    check_used_reg(R);
  }
#endif
}

// Fixme!
// Here we have handled neither depths, nor trim.
expression_ref subst_referenced_vars(const expression_ref& E, const vector<int>& Env, const map<int, expression_ref>& names)
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
  else if ( is_a<index_var>(E) )
  {
    const auto loc = names.find( lookup_in_env(Env, as_<index_var>(E).index) );
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
  const closure& C = H.access(R).C;

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

  names[R] = subst_referenced_vars(C.exp, C.Env, names);
}

string escape(const string& s)
{
  string s2;
  s2.resize(s.size()*2);
  int l=0;
  for(int i=0;i<s.size();i++)
  {
    if (s[i] == '\n')
    {
      s2[l++] = '\\';
      s2[l++] = 'n';
      continue;
    }

    bool escape_next = (s[i] == '\\') or (s[i] == '\n') or (s[i] == '"') or (s[i] == '<') or (s[i] == '>') or (s[i] == '{') or (s[i] == '}');

    if (escape_next)
      s2[l++] = '\\';
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
    if (is_a<reg_var>(E))
    {
      auto loc = ids.find(as_<reg_var>(E).target);
      if (loc != ids.end())
	return identifier(loc->second);
      else
	return E;
    }
    else
      return E;
  }

  object_ptr<expression> V = E.clone_expression();
  for(int i=0;i<E.size();i++)
    V->sub[i] = untranslate_vars(V->sub[i], ids);
  return V;
}

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, int>& ids)
{
  return C[R].C.exp;

  map< int, expression_ref> names;
  for(const auto& id: ids)
  {
    int R = id.second;
    string name = id.first;
    names[R] = expression_ref(new identifier(name) );
  }
  discover_graph_vars(C, R, names, ids);

  return launchbury_unnormalize(names[R]);
}

map<int,string> get_register_names(const map<string, int>& ids)
{
  map<int,string> ids2;
  for(const auto i:ids)
    ids2[i.second] = i.first;
  return ids2;
}

set<string> get_names(const map<string, int>& ids)
{
  set<string> names;
  for(const auto i:ids)
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
    if (reg_names.count(R)) continue;

    if (is_index_var(C.access(R).C.exp)) continue;

    if (is_modifiable(C.access(R).C.exp)) continue;

    if (C.access(R).C.exp.size() == 0)
    {
      string name = C.access(R).C.exp.print();
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

   2. Allow reduction result (call result) on the same level as redex.

 */

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o)
{
  const auto& ids = C.get_identifiers();

  map<int,string> reg_names = get_register_names(ids);

  const auto& params = C.get_parameters();
  for(const auto& p: params)
    reg_names[p.second] = p.first;

  map<string,string> simplify = get_simplified_names(get_names(ids));

  map<int,string> constants = get_constants(C, t);

  vector<int> regs = C.find_all_used_regs_in_context(t,false);

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

    expression_ref F = C.access(R).C.exp;

    bool print_record = false;
    if (F.head().ptr()->type() == operation_type or F.head().ptr()->type() == constructor_type)
    {
      if (not is_a<Case>(F) and not is_a<Apply>(F))
      {
	print_record = true;
	o<<"shape = record, ";
      }
    }

    // node label = R/name: expression
    string label = convertToString(R);
    if (reg_names.count(R))
      label += "/" + reg_names[R];
    label += ": ";

    vector<int> targets;
    if (print_record)
    {
      label = escape(label);

      label += " |";
      label += escape(F.head().print());
      if (F.is_expression())
	for(const expression_ref& E: F.sub())
	{
	  int index = E.as_<index_var>().index;
	  int R2 = C.access(R).C.lookup_in_env( index );
	  targets.push_back(R2);
	  
	  string reg_name = "<" + convertToString(R2) + ">";
	  if (reg_names.count(R2))
	  {
	    reg_name = reg_names[R2];
	    auto loc = simplify.find(reg_name);
	    if (loc != simplify.end())
	      reg_name = loc->second;
	  }
	  else if (constants.count(R2))
	    reg_name = constants[R2] + " " + reg_name;
	  label += "| <" + convertToString(R2) + "> " + escape(reg_name) + " ";
	}
    }
    else if (F.head().ptr()->type() == index_var_type)
    {
      int index = as_<index_var>(F).index;

      int R2 = C.access(R).C.lookup_in_env( index );

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
	
      //      expression_ref E = unlet(untranslate_vars(deindexify(trim_unnormalize(C.access(R).C)), reg_names));
      //      E = map_symbol_names(E, simplify);
      //      label += E.print();
      label = escape(wrap(label,40));
    }
    else
    {
      expression_ref E = unlet(untranslate_vars(untranslate_vars(deindexify(trim_unnormalize(C.access(R).C)), reg_names),constants));

      E = map_symbol_names(E, simplify);

      label += E.print();
      label = escape(wrap(label,40));
    }

    o<<"label = \""<<label<<"\"";
    if (C.access(R).n_heads)
      o<<",style=\"dashed,filled\",color=orange";
    else if (C.access(R).re_evaluate)
      o<<",style=\"dashed,filled\",color=yellow";
    else if (C.reg_is_changeable(R))
      o<<",style=\"dashed,filled\",color=red";

    if (C.reg_is_changeable(R) and C.reg_has_computation_result(R))
      o<<",fillcolor=\"#007700\",fontcolor=white";
    else if (C.reg_is_changeable(R))
      o<<",fillcolor=\"#770000\",fontcolor=white";
    else if (C.access(R).C.exp.head().ptr()->type() == index_var_type)
      o<<",fillcolor=\"#77bbbb\"";
    else if (C.reg_is_constant(R))
      o<<",fillcolor=\"#bbbb77\"";
    o<<"];\n";

    // out-edges
    if (print_record)
    {
      for(int R2: targets)
      {
	if (not C.is_used(R2)) continue;

	string name2 = "n" + convertToString(R2);
	bool used = false;
	for(int i: C.used_regs_for_reg(R))
	  if (i == R2) used = true;

	// Don't draw ref edges to things like fmap.
	if (reg_names.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

	// Don't draw ref edges to things like fmap.
	if (constants.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

	if (not used)
	  o<<name<<":<"<<R2<<">:s -> "<<name2<<":n;\n";
	else
	  o<<name<<":<"<<R2<<">:s -> "<<name2<<":n [color=\"#007777\"];\n";
      }
    }
    else
    {
      for(int R2: C.access(R).C.Env)
      {
	if (not C.is_used(R2)) continue;

	string name2 = "n" + convertToString(R2);
	bool used = false;
	for(int i: C.used_regs_for_reg(R))
	  if (i == R2) used = true;

	// Don't draw ref edges to things like fmap.
	if (reg_names.count(R2) and not C.reg_is_changeable(R2) and not used) continue;
	
	// Don't draw ref edges to things like fmap.
	if (constants.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

	if (not used)
	  o<<name<<":s -> "<<name2<<":n;\n";
	else
	  o<<name<<":s -> "<<name2<<":n [color=\"#007777\"];\n";
      }
    }

    // call-edges
    // FIXME:Drawing - how can allow these to go to the right, but not above, if no ref edges?
    // FIXME:Drawing - doing :w and {rank=same; n -> n} makes the edge drawn over the node icon.
    if (C.reg_has_call(R))
    {
      string name2 = "n" + convertToString(C.call_for_reg(R));
      o<<name<<":e -> "<<name2<<":w ";
      o<<"[";
      o<<"color=\"#007700\"";
      o<<"];\n";
    }

    // used_inputs
    for(int R2: C.used_regs_for_reg(R))
    {
      bool is_ref_edge_also = false;
      for(int R3: C.access(R).C.Env)
	if (R2 == R3)
	  is_ref_edge_also = true;

      if (is_ref_edge_also) continue;

      string name2 = "n" + convertToString(R2);
      o<<name<<":s -> "<<name2<<":n ";
      o<<"[";
      o<<"color=\"#007777\"";
      o<<",style=dashed";
      o<<"];\n";
    }

  }
  o<<"}"<<std::endl;
}

