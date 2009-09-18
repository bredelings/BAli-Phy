#include <fstream>
#include "tree-dist.H"

using std::vector;
using std::valarray;

using std::ifstream;
using std::string;
using std::endl;
using std::cerr;
using std::cout;
using std::istream;

using boost::dynamic_bitset;
using boost::shared_ptr;

namespace trees_format 
{
  const vector<string>& reader_t::names() const
  {
    return leaf_names;
  }

  int reader_t::lines() const {
    return lines_;
  }
  
  bool reader_t::next_tree(Tree& T)
  {
    int r;
    if (next_tree_(T,r)) {
      lines_++;
      return true;
    }
    else
      return false;
  }

  bool reader_t::next_tree(RootedTree& T)
  {
    int r;
    Tree& T2 = static_cast<Tree&>(T);
    if (next_tree_(T2,r)) {
      lines_++;
      T.reroot(r);
      return true;
    }
    else
      return false;
  }
  
  bool reader_t::next_tree(SequenceTree& T)
  {
    T.get_sequences() = leaf_names;
    
    return next_tree(static_cast<Tree&>(T));
  }
  
  bool reader_t::next_tree(RootedSequenceTree& T)
  {
    T.get_sequences() = leaf_names;
    
    return next_tree(static_cast<RootedTree&>(T));
  }
  
  bool Newick::next_tree_(Tree& T,int& r)
  {
    if (not line.size())
      while (getline(*file,line) and not line.size());
    if (not line.size()) return false;
    try {
      r = T.parse_with_names(line,leaf_names);
    }
    catch (std::exception& e) {
      cerr<<" Error! "<<e.what()<<endl;
      cerr<<" Quitting read of tree file."<<endl;
      file->setstate(std::ios::badbit);
      return false;
    }
    line.clear();
    return not done();
  }


  bool Newick::skip(int n)
  {
    for(int i=0;i<n and *file;i++)
      getline(*file,line);
    line.clear();
    return not done();
  }

  bool Newick::done() const
  {
    return (not *file);
  }

  void Newick::initialize()
  {
    SequenceTree T;
    getline(*file,line);
    T.parse(line);
    leaf_names = T.get_sequences();
    std::sort(leaf_names.begin(),leaf_names.end());
    
    //FIXME - this loses the first line!
  }

  Newick::Newick(const std::string& filename)
    :file(new ifstream(filename.c_str()))
  {
    initialize();
  }
  
  Newick::Newick(istream& i)
    :file(&i)
  {
    initialize();
  }

  Newick::~Newick()
  {}

  bool NEXUS::skip(int n)
  {
    for(int i=0;i<n and *file;i++)
      getline(*file,line,';');
    line.clear();
    return not done();
  }

  bool NEXUS::done() const
  {
    return (not *file);
  }

  istream& get_NEXUS_command(istream& file,std::string& s)
  {
    static const string eol = "\n\r";

    istream& is = getline(file,s,';');
    s = strip(s,eol);
    return is;
  }

  void NEXUS_skip_ws(int& i, const string& s)
  {
    static const string whitespace = "\t\n\r ";

    bool in_comment = false;

    while(contains_char(whitespace,s[i]) or in_comment or s[i]=='[') 
      {
	if (s[i] == '[')
	  in_comment = true;
	else if (s[i] == ']')
	  in_comment = false;

	i++;

	if (i >= s.size()) 
	  return;
      } 
  }


  bool get_word_NEXUS(string& word, int& i, const string& s)
  {
    //  static const string delimiters = "()[]{}/\\,;:=*`'\"+-<>";
    // REMOVE apostrophe... how to handle this?
    static const string delimiters = "(){}/\\,;:=*`\"+-<>";
    static const string whitespace = "\t\n\r ";

    if (i >= s.size()) 
      return false;

    NEXUS_skip_ws(i,s);

    if (i >= s.size()) 
      return false;

    int start = i;
    if (contains_char(delimiters,s[i])) {
      word = s.substr(i,1);
      i++;
      return true;
    }

    do { i++; }
    while(not contains_char(delimiters,s[i]) and not contains_char(whitespace,s[i])
	  and i < s.size());

    word = s.substr(start,i-start);

    return true;
  }

  vector<string> NEXUS_parse_line(const string& line)
  {
    string word;
    vector<string> words;
    int pos=0;
    while (get_word_NEXUS(word,pos,line))
      words.push_back(word);

    return words;
  }

  bool NEXUS::next_tree_(Tree& T,int& r)
  {
    if (not line.size())
      get_NEXUS_command(*file,line);
    if (not line.size()) return false;
    try {
      string word;
      int pos=0;
      get_word_NEXUS(word,pos,line);
      if (word == "end" or word == "END") {
	file->setstate(std::ios::badbit);
	return false;
      }
      get_word_NEXUS(word,pos,line);
      if (not (word == "=")) {
	get_word_NEXUS(word,pos,line);
	assert(word == "=");
      }
      NEXUS_skip_ws(pos,line);
      
      if (leaf_names.size())
	r = T.parse_with_names(line.substr(pos,line.size()-pos),leaf_names);
      else
	r = T.parse_no_names(line.substr(pos,line.size()-pos));
    }
    catch (std::exception& e) {
      cerr<<" Error! "<<e.what()<<endl;
      cerr<<" Quitting read of tree file."<<endl;
      file->setstate(std::ios::badbit);
      return false;
    }
    line.clear();
    return not done();
  }

  void NEXUS::parse_translate_command(const std::string& s)
  {
    translate = true;
  
    //  cerr<<"translating: "<<line<<endl;
  
    vector<string> words = NEXUS_parse_line(s);

    if (words.size()%3 != 2)
      throw myexception()<<"Malformed 'TRANSLATE' command: wrong number of tokens.";

    leaf_names.resize(words.size()/3+1);
    for(int i=0;i<leaf_names.size();i++) 
      {
	leaf_names[i] = words[i*3+1];

	if (i>0) assert(words[i*3-1] == ",");
      }
  }

  void NEXUS::initialize()
  {
    // Check #NEXUS
    getline(*file,line);
    if (line != "#NEXUS")
      throw myexception()<<"NEXUS trees reader: File does not begin with '#NEXUS' and may not be a NEXUS file.";
  
    // [ and ] do not break words

    string word;

    bool in_trees_block=false;

    while(get_NEXUS_command(*file,line))
    {
      //    cerr<<"line: "<<line<<endl;
      int pos=0;
      
      if (not get_word_NEXUS(word,pos,line)) continue;

      //      cerr<<"NEXUS: command = :"<<word<<":"<<"     in_trees_block = "<<in_trees_block<<endl;
      
      // Parse BEGIN TREES
      if (word == "BEGIN" or word == "begin") {
	if (not get_word_NEXUS(word,pos,line)) continue;
	if (word == "trees" or word == "TREES")
	  in_trees_block = true;
      }
      
      if (not in_trees_block) continue;
      
      // Parse TRANSLATE ...
      if (word == "translate" or word == "TRANSLATE") {
	parse_translate_command(line.substr(pos,line.size()-pos));
	//      cerr<<"leaf names = "<<join(leaf_names,',')<<endl;
	line.clear();
	return;
      }
      else if (word == "tree" or word == "TREE") {
	try {
	  get_word_NEXUS(word,pos,line);
	  if (not (word == "="))
	    get_word_NEXUS(word,pos,line);
	  NEXUS_skip_ws(pos,line);
	  SequenceTree T;
	  T.parse(line.substr(pos,line.size()-pos));
	  leaf_names = T.get_sequences();
	  std::sort(leaf_names.begin(),leaf_names.end());
	  return;
	}
	catch (std::exception& e) {
	  cerr<<" Error! "<<e.what()<<endl;
	  cerr<<" Quitting read of tree file."<<endl;
	  file->setstate(std::ios::badbit);
	}
      }
    }
  }

  NEXUS::NEXUS(const std::string& filename)
    :file(new ifstream(filename.c_str())),translate(false)
  {
    initialize();
  }

  NEXUS::NEXUS(istream& f)
    :file(&f),translate(false)
  {
    initialize();
  }

  NEXUS::~NEXUS()
  {}

  bool wrapped_reader_t::next_tree_(Tree& T,int& r) {
    if (tfr->next_tree_(T,r)) {
      lines_++;
      return true;
    }
    else
      return false;
  }

  const vector<string>& wrapped_reader_t::names() const
  {
    return tfr->names();
  }

  bool wrapped_reader_t::skip(int i) {
    return tfr->skip(i);
  }

  bool wrapped_reader_t::done() const {
    return tfr->done();
  }

  wrapped_reader_t::wrapped_reader_t() {}

  wrapped_reader_t::wrapped_reader_t(const reader_t&r) 
    :tfr(r.clone())
  { }


  Newick_or_NEXUS::Newick_or_NEXUS(const string& filename)
  {
    std::ifstream file(filename.c_str());

    if (file.peek() == '#')
      tfr = shared_ptr<reader_t>(new NEXUS(filename));
    else
      tfr = shared_ptr<reader_t>(new Newick(filename));

    file.close();
  }

  Newick_or_NEXUS::Newick_or_NEXUS(istream& file)
  {
    if (file.peek() == '#') 
      tfr = shared_ptr<reader_t>(new NEXUS(file));
    else
      tfr = shared_ptr<reader_t>(new Newick(file));
  }

  const vector<string>& Prune::names() const
  {
    return leaf_names;
  }

  bool Prune::next_tree_(Tree& T,int &r)
  {
    bool success = tfr->next_tree_(T,r);
    if (success and prune_index.size()) {
      T.prune_leaves(prune_index);
      // FIXME -- there has to be a more intelligent way to do this...
      r = T.n_nodes()-1;
    }
    return success;
  }

  Prune::Prune(const vector<string>& p,const reader_t& r)
    :wrapped_reader_t(r),prune(p)
  {
    vector<string> all_names = tfr->names();
    for(int i=0;i<all_names.size();i++) {
      if (not includes(prune,all_names[i]))
	leaf_names.push_back(all_names[i]);
    }

    for(int i=0;i<prune.size();i++) {
      int index = find_index(all_names,prune[i]);
      if (index == -1)
	throw myexception()<<"Cannot find leaf '"<<prune[i]<<"' in sampled tree.";
      prune_index.push_back(index);
    }
  }

  Skip::Skip(int skip, const reader_t& r)
    :wrapped_reader_t(r)
  {
    wrapped_reader_t::skip(skip);
  }

  bool Subsample::next_tree_(Tree& T,int& r)
  {
    bool success = wrapped_reader_t::next_tree_(T,r);
    wrapped_reader_t::skip(subsample-1);
    return success;
  }

  Subsample::Subsample(int s, const reader_t& r)
    :wrapped_reader_t(r),subsample(s)
  { }

  bool Max::next_tree_(Tree& T,int& r)
  {
    if (wrapped_reader_t::lines() < m)
      return wrapped_reader_t::next_tree_(T,r);
    else
      return false;
  }

  Max::Max(int i, const reader_t& r)
    :wrapped_reader_t(r),m(i)
  { }

  bool Fixroot::next_tree_(Tree& T,int& r)
  {
    if (wrapped_reader_t::next_tree_(T,r)) {
      if (T[r].degree() == 2) {
	T.remove_node_from_branch(r);
	r = T.n_nodes()-1;
      }
      return true;
    }
    else
      return false;
  }

  Fixroot::Fixroot(const reader_t& r)
    :wrapped_reader_t(r)
  { }
}

SequenceTree tree_sample::T(int i) const {
  return get_mf_tree(leaf_names,topologies[i].partitions);
}

valarray<bool> tree_sample::support(const string& t) const 
{
  typeof(index.begin()) here = index.find(t);

  if (here == index.end())
    return valarray<bool>(false,size());

  int t_i = index[t];

  valarray<bool> result(size());

  for(int i=0;i<result.size();i++)
    result[i] = (t_i == which_topology[i]);

  return result;
}

valarray<bool> tree_sample::support(const Partition& p) const 
{
  valarray<bool> result(size());

  for(int i=0;i<result.size();i++) 
  {
    // Get a tree with the same topology
    const vector<dynamic_bitset<> > & T = topologies[ which_topology[i] ].partitions;
    
    result[i] = implies(T,p);
  }
  return result;
}

valarray<bool> tree_sample::support(const vector<Partition>& partitions) const 
{
  valarray<bool> result(size());

  for(int i=0;i<result.size();i++) 
  {
    // Get a tree with the same topology
    const vector<dynamic_bitset<> >& T = topologies[ which_topology[i] ].partitions;
    
    result[i] = implies(T,partitions);
  }
  return result;
}

unsigned tree_sample::count(const Partition& P) const 
{
  unsigned count=0;
  for(int t=0;t<topologies.size();t++) 
    if (implies(topologies[t].partitions,P))
	count += topologies[t].count;
   
  return count;
}

unsigned tree_sample::count(const vector<Partition>& partitions) const 
{
  unsigned count=0;
  for(int t=0;t<topologies.size();t++) {
    if (implies(topologies[t].partitions,partitions))
      count += topologies[t].count;
  }
   
  return count;
}

double tree_sample::PP(const Partition& P) const 
{
  return double(count(P))/size();
}

double tree_sample::PP(const vector<Partition>& partitions) const 
{
  return double(count(partitions))/size();
}

struct ordering {
  const vector<tree_sample::topology_record>& v;

  // decreasing order of count
  bool operator()(int i,int j) {return v[i].count > v[j].count;}
  
  ordering(const vector<tree_sample::topology_record>& v_):v(v_) {}
};


tree_sample::topology_record::topology_record(const Tree& T,
					      const string& s)
  :topology(s),
   partitions(T.n_branches()-T.n_leafbranches()),
   count(0)
{ 
  const int L = T.n_leafbranches();
  for(int i=L;i<T.n_branches();i++)
    partitions[i-L] = branch_partition(T,i);
}


void tree_sample::add_tree(Tree& T)
{
  //------------ check tree ---------------//
  if (has_sub_branches(T))
    throw myexception()<<"Tree has node of degree 2";

  // Compute the standardized representation
  T.standardize();
  string t = write(T,leaf_names,false);
      
  // If it hasn't been seen before, insert it
  if (index.find(t) == index.end()) {
    topologies.push_back(topology_record(T,t));
    
    index[t] = topologies.size()-1;              // add to map of  (topology->index)
  }

  //FIXME - I'm doing the index[t] lookup twice;
      
  //----------- Add tree to distribution -------------//
  int i = index[t];
  which_topology.push_back(i);
  topologies[i].count++;
}

void tree_sample::add_tree(RootedTree& T)
{
  if (T.root().degree() == 2)
    T.remove_node_from_branch(T.root());

  add_tree(static_cast<Tree&>(T));
}

// What we actually want is a standardized STRING representation.
//  - We need to have ways of *reading* a Newick or NEXUS file and then
//  - ... converting to some intermediate record structure..
//  - ... which in this case in the standard string.

// We should be able to avoid sorting the leaves with every tree... in both cases!

// Reading an individual tree requires (a) one line, and (b) some state: ordered leaf names.

// Because we have to prune everything anyway, let's make the standard intermediate format
// .. be a (unrooted) Tree -- the names should be determined and given beforehand.



void tree_sample::load_file(istream& file,int skip,int max,int subsample,const vector<string>& prune)
{
  using namespace trees_format;

  //----------- Construct File Reader / Filter -----------//
  shared_ptr<reader_t> trees(new Newick_or_NEXUS(file));

  if (skip > 0)
    trees = shared_ptr<reader_t>(new Skip(skip,*trees));

  if (subsample > 1)
    trees = shared_ptr<reader_t>(new Subsample(subsample,*trees));

  if (max > 0)
    trees = shared_ptr<reader_t>(new Max(max,*trees));

  trees = shared_ptr<reader_t>(new Fixroot(*trees));

  if (prune.size())
    trees = shared_ptr<reader_t>(new Prune(prune,*trees));

  leaf_names = trees->names();

  //------------------- Process Trees --------------------//
  RootedTree T;
  while (trees->next_tree(T))
    add_tree(T);

  if (size() == 0)
    throw myexception()<<"No trees were read in!";
  
  cout<<"# n_trees = "<<size()<<"   n_topologies = "<<topologies.size()<<endl;
    
  //---------------  Sort topologies by count  ---------------//
  order.resize(topologies.size());
  for(int i=0;i<order.size();i++)
    order[i] = i;
  
  sort(order.begin(),order.end(),ordering(topologies));
}

tree_sample::tree_sample(istream& file,int skip,int max,int subsample,const vector<string>& prune)
{
  load_file(file,skip,max,subsample,prune);
}

tree_sample::tree_sample(const string& filename,int skip,int max,int subsample,const vector<string>& prune)
{
  ifstream file(filename.c_str());
  if (not file)
    throw myexception()<<"Couldn't open file "<<filename;
  
  load_file(file,skip,max,subsample,prune);
  file.close();
}

void scan_trees(istream& file,int skip,int subsample,int max,
		accumulator<SequenceTree>& op)
{
  using namespace trees_format;

  //----------- Construct File Reader / Filter -----------//
  shared_ptr<reader_t> trees(new Newick_or_NEXUS(file));

  if (skip > 0)
    trees = shared_ptr<reader_t>(new Skip(skip,*trees));

  if (subsample > 1)
    trees = shared_ptr<reader_t>(new Subsample(subsample,*trees));

  if (max > 0)
    trees = shared_ptr<reader_t>(new Max(max,*trees));

  trees = shared_ptr<reader_t>(new Fixroot(*trees));

  //------------------- Process Trees --------------------//
  RootedSequenceTree T;
  while (trees->next_tree(T))
    op(T);

  //---------------------- Finalize ----------------------//
  op.finalize();
}
