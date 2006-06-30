#include <iostream>
#include <map>
#include "tree-util.H"
#include "myexception.H"

using boost::program_options::variables_map;
using std::map;
using std::string;
using std::vector;
using std::valarray;
using std::istream;
using std::cout;
using std::cerr;
using std::endl;

/// Load a tree from command line args "--tree filename"
RootedSequenceTree load_T(const variables_map& args) {
  if (not args.count("tree"))
    throw myexception()<<"Tree file not specified! (--tree <filename>)";
    
  RootedSequenceTree RT;
  RT.read(args["tree"].as<string>());

  return RT;
}

vector<string> load_lines(istream& file,int skip,int subsample, int max)
{
  vector<string> lines;

  string line;
  for(int line_number=0;getline(file,line);line_number++) 
  {
    // don't start if we haven't skipped enough trees
    if (line_number < skip) continue;

    // skip trees unless they are a multiple of 'subsample'
    if ((line_number-skip) % subsample != 0) continue;

    // quit if we've read in 'max' trees
    if (max >= 0 and lines.size() == max) break;

    lines.push_back(line);
  }

  return lines;
}

vector<SequenceTree> load_trees(const vector<string>& lines) 
{
  if (lines.size() == 0)
    throw myexception()<<"No trees were read in!";
  
  vector<SequenceTree> trees;

  for(int i=0;i<lines.size();i++) 
  {
    RootedSequenceTree T;
    try {
      T.parse(lines[i]);
    }
    catch (std::exception& e) {
      cerr<<"Exception: "<<e.what()<<endl;
      cerr<<" Quitting read of tree file"<<endl;
      break;
    }

    trees.push_back(T);
  }

  return trees;
}

vector<SequenceTree> load_trees(istream& file,int skip,int subsample,int max)
{
  vector<string> lines = load_lines(file,skip,subsample,max);
  return load_trees(lines);
}

bool 
compare_complete_partitions::operator()(const std::valarray<bool>& p1,
					const std::valarray<bool>& p2) const
{
  assert(p1.size() == p2.size());
  
  for(int i=0;i<p1.size();i++) {
    if (p2[i] and not p1[i])
      return true;
    if (p1[i] and not p2[i])
      return false;
  }
  return false;
}

bool extends(const Tree& T,const Tree& Q)
{
  assert(T.n_leaves() == Q.n_leaves());

  if (T.n_branches() < Q.n_branches())
    return false;

  typedef map<valarray<bool>,int,compare_complete_partitions > container_t;
  container_t branches;

  // create leaves-only mask to use as temp var
  std::valarray<bool> partition(T.n_leaves());

  // insert informative partitions of T into 'branches'
  for(int b=T.n_leafbranches();b<T.n_branches();b++) 
  {
    partition = T.partition(b);
    if (not partition[0])
      partition = (not partition);
    
    branches.insert(container_t::value_type(partition,b));
  };  
  
  // check if informative partitions of Q are in 'branches'
  bool fail = false;
  for(int b=Q.n_leafbranches();b<Q.n_branches();b++) 
  {
    partition = Q.partition(b);
    if (not partition[0])
      partition = (not partition);

    container_t::iterator record = branches.find(partition);
    if (record == branches.end())
      fail = true;
  }
  return not fail;
}

vector<int> extends_map(const Tree& T,const Tree& Q)
{
  assert(T.n_leaves() == Q.n_leaves());

  if (T.n_branches() < Q.n_branches())
    return vector<int>();

  // set up the branch map
  vector<int> branch_map(Q.n_branches()*2,-1);
  for(int i=0;i<Q.n_leafbranches();i++) 
  {
    const_branchview Qb = Q.directed_branch(i);
    const_branchview Tb = T.directed_branch(i);

    assert(Qb.name() == i);
    
    branch_map[Qb.name()] = Tb.name();
    branch_map[Qb.reverse().name()] = Tb.reverse().name();
  }
    

  typedef map<valarray<bool>,int,compare_complete_partitions > container_t;
  container_t branches;

  // create leaves-only mask to use as temp var
  std::valarray<bool> partition(T.n_leaves());

  // insert informative partitions of T into 'branches'
  for(int b=T.n_leafbranches();b<T.n_branches();b++) 
  {
    partition = T.partition(b);
    if (not partition[0])
      partition = (not partition);
    
    branches.insert(container_t::value_type(partition,b));
  };  
  
  // check if informative partitions of Q are in 'branches'
  bool fail = false;
  for(int b=Q.n_leafbranches();b<Q.n_branches();b++) 
  {
    partition = Q.partition(b);
    if (not partition[0])
      partition = (not partition);

    container_t::iterator record = branches.find(partition);
    if (record == branches.end())
      fail = true;
    else {
      int b2 = (*record).second;

      const_branchview Qb = Q.directed_branch(b);
      const_branchview Tb = T.directed_branch(b2);

      if (T.partition(b2)[0] != Q.partition(b)[0])
	Tb = Tb.reverse();
      
      branch_map[Qb.name()] = Tb.name();
      branch_map[Qb.reverse().name()] = Tb.reverse().name();
    }
  }

  if (fail)
    return vector<int>();
  else 
  {
    for(int i=0;i<branch_map.size();i++)
      assert(branch_map[i] != -1);
    
    valarray<bool> partition1(T.n_leaves());
    valarray<bool> partition2(Q.n_leaves());
    for(int i=0;i<branch_map.size();i++) {
      int b1 = branch_map[i];
      int b2 = i;
      
      partition1 = T.partition(b1);
      partition2 = Q.partition(b2);
      
      assert(equal(partition1,partition2));
    }

    return branch_map;
  }
}
