#include "alignment-util.H"
#include "util.H"
#include "setup.H"

using std::vector;
using std::valarray;

using boost::program_options::variables_map;


alignment chop_internal(alignment A) 
{
  int N = (A.size2()+2)/2;

  bool internal_nodes = true;
  for(int i=N;i<A.size2();i++) {
    if (A.seq(i).name.size() == 0 or A.seq(i).name[0] != 'A') {
      internal_nodes = false; 
      break;
    }
    for(int column=0;column<A.length();column++) {
      if (alphabet::letter( A(column,i) )) {
	internal_nodes = false; 
	break;
      }
    }
  }

  if (not internal_nodes)
    return A;

  while(A.n_sequences() > N)
    A.del_sequence(N);

  return A;
}

alignment add_internal(alignment A,const Tree& T) 
{
  // Complain if A and T don't correspond
  if (A.n_sequences() != T.n_leaves())
    throw myexception()<<"Number of sequence in alignment doesn't match number of leaves in tree"
		       <<"- can't add internal sequences";

  // Add empty sequences
  for(int i=T.n_leaves();i<T.n_nodes();i++) {
    sequence s;
    s.name = string("A") + convertToString(i);
    A.add_sequence(s);
  }

  // Set them to all wildcards
  for(int column=0;column<A.length();column++)
    for(int i=T.n_leaves();i<T.n_nodes();i++)
      A(column,i) = alphabet::not_gap;

  return A;
}


/// Construct a mapping of letters to columns for each leaf sequence
vector< vector<int> > column_lookup(const alignment& A,int nleaves) {
  if (nleaves == -1)
    nleaves = A.size2();

  vector< vector<int> > result;

  for(int i=0;i<nleaves;i++) {
    vector<int> columns;
    for(int column=0;column<A.length();column++) {
      if (not A.gap(column,i))
	columns.push_back(column);
    }
    result.push_back(columns);
  }

  return result;
}

/// Replace each letter with its position in its sequence
ublas::matrix<int> M(const alignment& A1) {
  ublas::matrix<int> A2(A1.length(),A1.size2());
  for(int i=0;i<A2.size2();i++) {
    int pos=0;
    for(int column=0;column<A2.size1();column++) {
      if (not A1.gap(column,i)) {
	A2(column,i) = pos;
	pos++;
      }
      else
	A2(column,i) = alphabet::gap;
    }

    assert(pos == A1.seqlength(i));

  }
  return A2;
}

/// Is the homology A1(column,s1)::A1(column,s2) preserved in A2 ?
bool A_match(const ublas::matrix<int>& M1, int column, int s1, int s2, 
	     const ublas::matrix<int>& M2,
	     const vector< vector< int> >& column_indices) {
  if (M1(column,s1) == alphabet::gap and M1(column,s2)==alphabet::gap)
    return true;

  // Turn this into a statement about what s1[column] matches
  if (M1(column,s1)==alphabet::gap)
    std::swap(s1,s2);

  // which column in A2 has the A1(column,s1)-th feature of s1 ?
  int column2 = column_indices[s1][ M1(column,s1) ];
  return (M2(column2,s2) == M1(column,s2));
}


bool A_constant(alignment A1, alignment A2, const valarray<bool>& ignore) {
  assert(A1.size2() == A2.size2());
  assert(ignore.size() == A1.size2());

  // convert to feature-number notation
  ublas::matrix<int> M1 = M(A1);
  ublas::matrix<int> M2 = M(A2);

  // lookup and cache the column each feature is in
  vector< vector< int> > column_indices = column_lookup(A2);

  //----- Check that the sequence lengths match ------//
  for(int i=0;i<M1.size2();i++) {
    if (ignore[i]) continue;

    if (A1.seqlength(i) != A2.seqlength(i))
      return false;
  }

  //----- Check that each homology in A1 is in A2 -----//
  for(int column=0; column<A1.length(); column++)
    for(int s1=0; s1 < A1.size2(); s1++) {
      if (ignore[s1]) continue;
      for(int s2=s1+1; s2 < A1.size2(); s2++) {
	if (ignore[s2]) continue;
	if (not A_match(M1,column,s1,s2,M2,column_indices))
	  return false;
      }
    }

  return true;
}

bool bit_set(const valarray<bool>& v) {
  for(int i=0;i<v.size();i++)
    if (v[i]) return true;
  return false;
}


/// Check that any two present nodes are connected by a path of present nodes
bool all_characters_connected(const Tree& T,valarray<bool> present,const vector<int>& _ignore) {
  assert(present.size() == T.n_nodes());

  //--------- set the ignored nodes to 'not present' -----------//
  valarray<bool> ignore(false,present.size());
  for(int i=0;i<_ignore.size();i++) {
    int n = _ignore[i];
    present[n] = false;
    ignore[n] = true;
  }

  //---------- for each internal node... -------------//
  for(int n1=T.n_leaves(); n1<T.n_nodes(); n1++) {

    if (present[n1] or ignore[n1]) continue;
      
    //------- if it is '-' and not ignored ... -------//
    vector<const_nodeview> neighbors;
    append(T[n1].neighbors(),neighbors);
    assert(neighbors.size() == 3);

    //---- check the three attatched subtrees ... ----//
    int total=0;
    for(int i=0;i<neighbors.size();i++) {
      valarray<bool> group = T.partition(n1,neighbors[i]);
      if (bit_set(present and group))
	total++;
    }

    //----- nodes should be present in only one. -----//
    if (total > 1)
      return false;
  }
  return true;
}


/// Check that internal nodes don't have letters (or anything wierder!)
void check_internal_sequences_composition(const alignment& A,int n_leaves) {

  for(int column=0;column<A.length();column++)
    for(int i=n_leaves;i<A.size2();i++) 
      if (A(column,i) == alphabet::gap)
	;
      else if (A(column,i) == alphabet::not_gap)
	;
      else
	throw myexception()<<"Found a illegal index "<<A(column,i)
			   <<"in column "<<column<<" of internal sequence '"
			   <<A.seq(i).name<<"': only - and * are allowed";
}

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const Tree& T,const vector<int>& ignore) {
  for(int column=0;column<A.length();column++) {
    valarray<bool> present(T.n_nodes());
    for(int i=0;i<T.n_nodes();i++) 
      present[i] = not A.gap(column,i);
    
    if (not all_characters_connected(T,present,ignore)) {
      std::cerr<<"Internal node states are inconsistent in column "<<column<<std::endl;
      std::cerr<<A<<std::endl;
      throw myexception()<<"Internal node states are inconsistent in column "<<column;
    }
  }
}

void letters_OK(const alignment& A) {
  check_letters_OK(A);
}

void check_letters_OK(const alignment& A) {
  const alphabet& a = A.get_alphabet();

  bool bad=false;
  for(int i=0;i<A.length();i++)
    for(int j=0;j<A.size2();j++)
      if (A(i,j) >=0 and A(i,j) < a.size())
	; // this is a letter
      else if (A(i,j) == alphabet::gap)
	; // this is a '-'
      else if (A(i,j) == alphabet::not_gap)
	; // this is a '*'
      else {
	bad = true;
	std::cerr<<"A("<<i<<","<<j<<") = "<<A(i,j)<<std::endl;
      }
  if (bad)
    std::abort();
}

void check_leaf_sequences(const alignment& A,int n_leaves) {

  vector<sequence> sequences = A.get_sequences();

  for(int i=0;i<n_leaves;i++) {

    sequences[i].strip_gaps();
    if (not (sequences[i] == A.seq(i))) {
      std::cerr<<"leaf sequence "<<i<<" corrupted!\n";

      std::cerr<<sequences[i]<<std::endl;

      std::cerr<<A.seq(i)<<std::endl;

      std::abort();
    }
  }
}

void check_alignment(const alignment& A,const Tree& T,bool internal_sequences) {
  // First check that there are no illegal letters
  check_letters_OK(A);

  // Next check that the internal sequences haven't changed
  check_leaf_sequences(A,T.n_leaves());

  if (not internal_sequences) return;

  // Next check that only * and - are found at internal nodes
  check_internal_sequences_composition(A,T.n_leaves());
  
  // Finally check that the internal node states are consistent
  check_internal_nodes_connected(A,T);
}

vector<const_branchview> branches_toward_from_node(const Tree& T,int n) {
  vector<const_branchview> branches;
  branches.reserve(2*T.n_branches());

  branches = branches_from_node(T,n);
  std::reverse(branches.begin(),branches.end());
  for(int i=0;i<T.n_branches();i++)
    branches.push_back(branches[i]);

  for(int i=0;i<T.n_branches();i++)
    branches[i] = branches[branches.size()-1-i].reverse();

  return branches; 
}


ublas::matrix<int> get_SM(const alignment& A,const Tree& T) {
  ublas::matrix<int> SM(A.length(),2*T.n_branches());
    
  vector<const_branchview> branches = branches_toward_from_node(T,T.n_leaves());

  // Compute the sub-alignments
  vector<const_branchview> temp;temp.reserve(2);
  for(int i=0;i<branches.size();i++) {
    int b = branches[i];


    int l=0;
    for(int c=0;c<SM.size1();c++) {
      SM(c,b) = alphabet::gap;

      // for leaf branches fill from the alignment
      if (branches[i].source().is_leaf_node()) {
	if (not A.gap(c,b))
	  SM(c,b) = l++;
      }

      // for internal branches fill from the previous branches
      else {
	temp.clear();
	append(T.directed_branch(b).branches_before(),temp);
	assert(temp.size() == 2);

	if (SM(c,temp[0]) != -1 or SM(c,temp[1]) != -1)
	  SM(c,b) = l++;
      }

    }
  }

  return SM;
}

long int asymmetric_pairs_distance(const alignment& A1,const alignment& A2) {

  ublas::matrix<int> M1 = M(A1);
  ublas::matrix<int> M2 = M(A2);

  // lookup and cache the column each feature is in
  vector< vector< int> > column_indices2 = column_lookup(A2);

  return asymmetric_pairs_distance(M1,M2,column_indices2);
}


long int asymmetric_pairs_distance(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
				    const vector< vector<int> >& column_indices2)
{
  int mismatch=0;

  for(int column=0;column<M1.size1();column++) 
    for(int i=0;i<M1.size2();i++)
      for(int j=0;j<M1.size2();j++)
	if (M1(column,i) != alphabet::gap or M1(column,j)!= alphabet::gap) {
	  if (not A_match(M1,column,i,j,M2,column_indices2))
	    mismatch++;
	}

  return mismatch;
}

vector<int> get_splitgroup_columns(const ublas::matrix<int>& M1,
				   int column,
				   const ublas::matrix<int>& M2,
				   const vector< vector<int> >& columns) 
{
  vector<int> label(M1.size2());
  for(int i=0;i<label.size();i++) {
    if (M1(column,i) == alphabet::gap)
      label[i] = -1;
    else
      label[i] = columns[i][M1(column,i)];
  }

  // If letter from the original column is in a column with a gap here
  // then put this gap in the same column as the letter
  for(int i=0;i<label.size();i++) {
    if (label[i] != -1) continue;
    for(int j=0;j<label.size() and label[i] == -1;j++) {
      if (label[j] != -1 and M2(label[j],i) == alphabet::gap)
	label[i] = label[j];
    }
  }
  

  return label;
}

long int asymmetric_splits_distance(const alignment& A1,const alignment& A2) 
{

  ublas::matrix<int> M1 = M(A1);
  ublas::matrix<int> M2 = M(A2);

  // lookup and cache the column each feature is in
  vector< vector< int> > column_indices2 = column_lookup(A2);

  return asymmetric_splits_distance(M1,M2,column_indices2);
}

long int asymmetric_splits_distance(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
				    const vector< vector<int> >& column_indices2)
{
  int match=0;

  for(int column=0;column<M1.size1();column++) {
    vector<int> columns = get_splitgroup_columns(M1,column,M2,column_indices2);

    vector<int> uniq;uniq.reserve(columns.size());
    for(int i=0;i<columns.size();i++)
      if (not includes(uniq,columns[i]))
	uniq.push_back(columns[i]);

    int splits = uniq.size();
    splits--;
    assert(splits >= 0);
    match += splits;
  }

  return match;
}

long int pairs_distance(const alignment& A1,const alignment& A2) 
{
  return asymmetric_pairs_distance(A1,A2) + asymmetric_pairs_distance(A2,A1);
}

long int pairs_distance(const ublas::matrix<int>& M1,const vector< vector<int> >& column_indices1,
			const ublas::matrix<int>& M2,const vector< vector<int> >& column_indices2)
{
  return asymmetric_pairs_distance(M1,M2,column_indices2)
    + asymmetric_pairs_distance(M2,M1,column_indices1);
}


long int splits_distance(const alignment& A1,const alignment& A2) 
{
  return asymmetric_splits_distance(A1,A2)+asymmetric_splits_distance(A2,A1);
}

long int splits_distance(const ublas::matrix<int>& M1,const vector< vector<int> >& column_indices1,
			const ublas::matrix<int>& M2,const vector< vector<int> >& column_indices2)
{
  return asymmetric_splits_distance(M1,M2,column_indices2)
    + asymmetric_splits_distance(M2,M1,column_indices1);
}


vector<OwnedPointer<alphabet> > load_alphabets(const variables_map& args) {
  OwnedPointer<AminoAcids> AA = AminoAcids();
  if (args.count("Use Stop"))
    AA = AminoAcidsWithStop();

  vector<OwnedPointer<alphabet> > alphabets; 
  if (args.count("alphabet") && args["alphabet"].as<string>() == "Codons") {
    {
      string dna_filename = args["datadir"].as<string>() + "/" + "genetic_code_dna.dat";
      alphabets.push_back(Codons(DNA(),*AA,dna_filename));
    }
    
    {
      string rna_filename = args["datadir"].as<string>() + "/" + "genetic_code_rna.dat";
      alphabets.push_back(Codons(RNA(),*AA,rna_filename));
    }
  }
  else {
    alphabets.push_back(DNA());
    alphabets.push_back(RNA());
    alphabets.push_back(*AA);
  }

  return alphabets;
}




/// Load an alignment from command line args "--align filename"
alignment load_A(const variables_map& args,bool keep_internal) 
{
  vector<OwnedPointer<alphabet> > alphabets = load_alphabets(args);
  
  // ----- Try to load alignment ------ //
  if (not args.count("align")) 
    throw myexception("Alignment file not specified! (--align <filename>)");
  
  alignment A;
  if (args["align"].as<string>() == "-")
    A.load(alphabets,sequence_format::read_guess,std::cin);
  else
    A.load(alphabets,args["align"].as<string>());
  
  remove_empty_columns(A);
  
  if (A.num_sequences() == 0)
    throw myexception()<<"Alignment file "<<args["align"].as<string>()<<" didn't contain any sequences!";

  if (not keep_internal)
    A = chop_internal(A);

  return A;
}


using std::vector;
using std::string;
using std::list;

bool match_tag(const string& line,const string& tag) {
  if (line.size() < tag.size())
    return false;

  return (line.substr(0,tag.size()) == tag);
}


list<alignment> load_alignments(std::istream& ifile, const string& tag,
				  const vector<OwnedPointer<alphabet> >& alphabets, int maxalignments) {
  list<alignment> alignments;
  
  // we are using every 'skip-th' alignment
  int skip = 1;

  alignment A;
  string line;
  for(int nth=0;getline(ifile,line);) {
    
    // Continue with the next line IF no alignment begins here
    if (not match_tag(line,tag)) continue;

    // Increment the counter SINCE we saw an alignment
    nth++;

    // Skip this alignment IF it isn't the right multiple
    if (nth%skip != 0) continue;

    // READ the next alignment
    try {
      if (not alignments.size())
	A.load(alphabets,sequence_format::read_guess,ifile);
      else 
	ifile>>A;
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }

    // strip out empty columns
    remove_empty_columns(A);

    // complain if there are no sequences in the alignment
    if (A.num_sequences() == 0) 
      throw myexception(string("Alignment didn't contain any sequences!"));
    
    // STORE the alignment if we're not going to skip it
    alignments.push_back(A);

    // If there are too many alignments
    if (alignments.size() > 2*maxalignments) {
      // start skipping twice as many alignments
      skip *= 2;

      std::cerr<<"Went from "<<alignments.size();
      // Remove every other alignment
      for(typeof(alignments.begin()) loc =alignments.begin();loc!=alignments.end();) {
	typeof(loc) j = loc++;

	alignments.erase(j);

	if (loc == alignments.end()) 
	  break;
	else
	  loc++;
      }
	
      std::cerr<<" to "<<alignments.size()<<" alignments.\n";

    }

  }

  // If we have too many alignments
  if (alignments.size() > maxalignments) {
    assert(alignments.size() < maxalignments*2);

    // We have this many extra alignments
    const int extra = alignments.size() - maxalignments;

    // Remove this many alignments from the array
    std::cerr<<"Went from "<<alignments.size();

    vector<int> kill(extra);
    for(int i=0;i<kill.size();i++)
      kill[i] = int( double(i+0.5)*alignments.size()/extra);
    std::reverse(kill.begin(),kill.end());

    int i=0;
    for(typeof(alignments.begin()) loc = alignments.begin();loc!=alignments.end();i++) {
      if (i == kill.back()) {
	kill.pop_back();
	typeof(loc) j = loc++;
	alignments.erase(j);
      }
      else
	loc++;
    }
    assert(kill.empty());
    std::cerr<<" to "<<alignments.size()<<" alignments.\n";
  }

  return alignments;
}

alignment find_first_alignment(std::istream& ifile, const string& tag,
			       const vector<OwnedPointer<alphabet> >& alphabets) 
{
  alignment A;

  // for each line (nth is the line counter)
  string line;
  while(getline(ifile,line)) {
    
    // read the next alignments only if we match the tag
    if (not match_tag(line,tag)) continue;

    try {
      // read alignment into A
      alignment A2;
      A2.load(alphabets,sequence_format::read_guess,ifile);
      A = A2;

      // strip out empty columns
      remove_empty_columns(A);
      break;
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }

  }

  if (A.num_sequences() == 0) 
    throw myexception(string("Couldn't find any alignments w/ tag ") + tag);

  return A;
}

alignment find_last_alignment(std::istream& ifile, const string& tag,
			      const vector<OwnedPointer<alphabet> >& alphabets) 
{
  alignment A;

  // for each line (nth is the line counter)
  string line;
  while(getline(ifile,line)) {
    
    // read the next alignments only if we match the tag
    if (not match_tag(line,tag)) continue;

    try {
      // read alignment into A
      alignment A2;
      A2.load(alphabets,sequence_format::read_guess,ifile);
      A = A2;

      // strip out empty columns
      remove_empty_columns(A);
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }
  }

  if (A.num_sequences() == 0) 
    throw myexception(string("Couldn't find any alignments w/ tag ") + tag);

  return A;
}
