#include <sstream>
#include "alignment-constraint.H"
#include "alignment-util.H"

using std::vector;
using std::valarray;

ublas::matrix<int> load_alignment_constraint(Arguments& args,const alignment& A,SequenceTree& T) {
  ublas::matrix<int> constraint(0,T.n_leaves());

  if (args.set("align-constraint")) {
    // Load constraint file
    ifstream constraint_file(args["align-constraint"].c_str());
    if (not constraint_file)
	throw myexception()<<"Couldn't open alignment-constraint file \""<<
	  args["align-constraint"]<<"\".";

    // Map columns to species
    string line;
    getline(constraint_file,line);
    vector<string> names = split(line,' ');
    vector<int> mapping = compute_mapping(names,T.get_sequences());

    // Load constraints
    int line_no=1;
    vector<vector<int> > constraints;
    while(getline(constraint_file,line)) {

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

bool group_present(const ublas::matrix<int>& constraint,int c,std::valarray<bool> group) {
  bool present = false;
  for(int i=0;i<constraint.size2();i++)
    if (group[i] and constraint(c,i) != -1)
      present = true;
  return present;
}


vector< vector<int> > get_pins(const ublas::matrix<int>& constraint,const alignment& A,
			       const valarray<bool>& group1,const valarray<bool>& group2,
			       const vector<int>& seq1,const vector<int>& seq2) 
{
  vector<vector<int> > column_indices = column_lookup(A);


  // determine which constraints are satisfied, and can be enforced
  vector<int> satisfied(constraint.size1(),-1);
  for(int i=0;i<constraint.size1();i++) {

    // this constraint has to apply to both halves of the alignment
    if (not (group_present(constraint,i,group1) and group_present(constraint,i,group2))) {
      std::cerr<<"constraint #"<<i+1<<" not relevant.\n";
      continue;
    }
    
    // check if all the constrained residues are in the same column
    int col = -1;
    bool same_column = true;
    for(int j=0;j<constraint.size2();j++) {
      if (constraint(i,j) == -1) continue;

      int this_col = column_indices[j][constraint(i,j)];
      if (col == -1)
	col = this_col;
      else
	if (col != this_col)
	  same_column = false;
    }
      
    // record the column if this constraint is to be enforced
    if (same_column) {
      std::cerr<<"constraint #"<<i+1<<" satisfied.\n";
      satisfied[i] = col;
    }
    else
      std::cerr<<"constraint #"<<i+1<<" NOT satisfied.\n";

  }
  

  // convert columns of enforced constraints to pairs of indices in seq1/seq2
  vector<vector<int> > pins(2);
  for(int i=0;i<constraint.size1();i++) {
    if (satisfied[i] == -1) continue;
    
    int x = find_index(seq1,satisfied[i]);
    int y = find_index(seq2,satisfied[i]);

    pins[0].push_back(x);
    pins[1].push_back(y);
  }

  return pins;
}
