#include <sstream>
#include <iostream>
#include "alignment-constraint.H"
#include "alignment-util.H"
#include "util.H"

using std::vector;
using std::valarray;

using boost::program_options::variables_map;

ublas::matrix<int> load_alignment_constraint(const variables_map& args,
					     const alignment& A,SequenceTree& T) 
{
  ublas::matrix<int> constraint(0,T.n_leaves());

  if (args.count("align-constraint")) {
    // Load constraint file
    ifstream constraint_file(args["align-constraint"].as<string>().c_str());
    if (not constraint_file)
	throw myexception()<<"Couldn't open alignment-constraint file \""<<
	  args["align-constraint"].as<string>()<<"\".";

    // Map columns to species
    string line;
    getline(constraint_file,line);
    vector<string> names = split(line,' ');
    vector<int> mapping = compute_mapping(names,T.get_sequences());

    // Load constraints
    int line_no=1;
    vector<vector<int> > constraints;
    while(getline(constraint_file,line)) {

      int loc = line.find('#');
      if (loc == -1)
	loc = line.length();

      while (loc > 0 and line[loc-1] == ' ')
	loc--;

      line = line.substr(0,loc);

      // lex contraint line
      vector<string> entries = split(line,' ');
      if (entries.size() != T.n_leaves())
	throw myexception()<<"constraint: line "<<line_no<<
	  " only has "<<entries.size()<<"/"<<T.n_leaves()<<" entries.";

      // parse contraint line
      vector<int> c_line(T.n_leaves());
      for(int i=0;i<entries.size();i++) {
	if (entries[i] == "-")
	  c_line[mapping[i]] = alphabet::gap;
	else
	  //FIXME - we should probably check that c_line[i] is in [0,length(i)-1]
	  c_line[mapping[i]] = convertTo<int>(entries[i]);
      }

      constraints.push_back(c_line);
      line_no++;
    }

    // load constraints into matrix
    constraint.resize(constraints.size(),T.n_leaves());
    for(int i=0;i<constraint.size1();i++)
      for(int j=0;j<constraint.size2();j++)
	constraint(i,j) = constraints[i][j];
  }

  return constraint;
}

bool constrained(const std::valarray<bool>& group,const ublas::matrix<int>& constraint,int c) 
{
  bool present = false;
  for(int i=0;i<constraint.size2();i++)
    if (group[i] and constraint(c,i) != -1)
      present = true;
  return present;
}

// This procedure bases the constraint columns ENTIRELY on the leaf sequence alignment!
// Therefore these constrained columns may be unalignable, depending on the internal node
//  states!
vector<int> constraint_columns(const ublas::matrix<int>& constraint,const alignment& A) 
{
  // determine which constraints are satisfied, and can be enforced
  vector<int> columns(constraint.size1(),-1);

  // get columns for each residue
  vector<vector<int> > column_indices = column_lookup(A);

  // for all constraints (i,*) != -1, check...
  for(int i=0;i<constraint.size1();i++) 
    for(int j=0;j<constraint.size2();j++) 
      if (constraint(i,j) != -1) 
      {
	int c = column_indices[j][constraint(i,j)];

	if (columns[i] == -1)
	  columns[i] = c;
	else if (columns[i] != c) {
	  columns[i] = -1;
	  break;
	}
      }

  return columns;
}

vector< vector<int> > get_pins(const ublas::matrix<int>& constraint,const alignment& A,
			       const valarray<bool>& group1,const valarray<bool>& group2,
			       const vector<int>& seq1,const vector<int>& seq2) 
{
  // determine which constraints are satisfied (not necessarily enforceable!)
  vector<int> satisfied = constraint_columns(constraint,A);

  // determine which of the satisfied constraints are enforceable
  for(int i=0;i<satisfied.size();i++) {
    // ignore columns in which all constrained residues are in either group1 or group2
    // we cannot enforce these constraints, and also cannot affect them
    if (not (constrained(group1,constraint,i) and constrained(group2,constraint,i)))
      satisfied[i] = -1;
  }

  // convert columns of enforced constraints to pairs of indices in seq1/seq2
  vector<vector<int> > pins(2);
  for(int i=0;i<constraint.size1();i++) {
    if (satisfied[i] == -1) continue;
    
    int x = find_index(seq1,satisfied[i]);
    int y = find_index(seq2,satisfied[i]);

    assert(x >=0 and x < seq1.size());
    assert(y >=0 and y < seq2.size());

    pins[0].push_back(x);
    pins[1].push_back(y);
  }

  return pins;
}


valarray<bool> constraint_satisfied(const ublas::matrix<int>& constraint,const alignment& A) 
{
  vector<int> columns = constraint_columns(constraint,A);

  valarray<bool> satisfied(columns.size());
  for(int i=0;i<satisfied.size();i++)
    satisfied[i] = columns[i] != -1;

  return satisfied;
}

static int sum(const valarray<bool>& v) {
  int count = 0;
  for(int i=0;i<v.size();i++)
    if (v[i]) count++;
  return count;
}

void report_constraints(const valarray<bool>& s1, const valarray<bool>& s2) {
  assert(s1.size() == s2.size());

  if (not s1.size()) return;

  for(int i=0;i<s1.size();i++) {
    if (s1[i] and not s2[i])
      throw myexception()<<"Constraint "<<i<<" went from satisfied -> unsatisfied!";
    if (s2[i] and not s1[i])
      std::cerr<<"Constraint "<<i<<" satisfied."<<std::endl;
  }
  if (sum(s1) != s1.size() and sum(s2) == s2.size())
    std::cerr<<"All constraints satisfied."<<std::endl;

#ifndef NDEBUG
  std::cerr<<sum(s1)<<"/"<<s2.size()<<" constraints satisfied.\n";
#endif
}
