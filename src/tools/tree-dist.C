#include <map>
#include <fstream>
#include "tree-dist.H"
#include "tree-util.H"
#include "rng.H"
#include "statistics.H"
#include "mytypes.H"
#include "util.H"

using std::vector;
using std::list;
using std::valarray;

using std::map;
using std::pair;

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
    return not done();
  }


  bool Newick::skip(int n)
  {
    for(int i=0;i<n and *file;i++)
      getline(*file,line);
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
      getline(*file,line);
    return not done();
  }

  bool NEXUS::done() const
  {
    return (not *file);
  }

  istream& get_NEXUS_command(istream& file,std::string& s)
  {
    return getline(file,s,';');
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
      
      if (translate)
	r = T.parse_no_names(line.substr(pos,line.size()-pos));
      else
	r = T.parse_with_names(line.substr(pos,line.size()-pos),leaf_names);
    }
    catch (std::exception& e) {
      cerr<<" Error! "<<e.what()<<endl;
      cerr<<" Quitting read of tree file."<<endl;
      file->setstate(std::ios::badbit);
      return false;
    }
    return not done();
  }

  void NEXUS::parse_translate_command(const std::string& line)
  {
    translate = true;
  
    //  cerr<<"translating: "<<line<<endl;
  
    vector<string> words = NEXUS_parse_line(line);

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

      //cerr<<"NEXUS: command = :"<<word<<":"<<"     in_trees_block = "<<in_trees_block<<endl;
      
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
   partitions(T.n_branches()),
   count(0)
{ 
  for(int i=0;i<T.n_branches();i++)
    partitions[i] = branch_partition(T,i);
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

tree_sample::tree_sample(istream& file,int skip,int max,int subsample,const vector<string>& prune)
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


struct p_count {
  int count;
  int last_tree;
  p_count(): count(0),last_tree(-1) {}
};

vector<pair<Partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l,const dynamic_bitset<>&  mask) 
{
  // find the first bit
  int first = mask.find_first();

  if (l <= 0.0)
    throw myexception()<<"Consensus level must be > 0.0";
  if (l > 1.0)
    throw myexception()<<"Consensus level must be <= 1.0";

  // use a sorted list of <partition,count>, sorted by partition.
  typedef map<dynamic_bitset<>,p_count> container_t;
  container_t counts;

  // use a linked list of pointers to <partition,count> records.
  list<container_t::iterator> majority;

  vector<string> names = sample.names();

  unsigned count = 0;

  for(int i=0;i<sample.topologies.size();i++) 
  {
    const vector<dynamic_bitset<> >& T = sample.topologies[i].partitions;

    unsigned delta = sample.topologies[i].count;

    unsigned min_old = std::min(1+(unsigned)(l*count),count);

    count += delta;
    unsigned min_new = std::min(1+(unsigned)(l*count),count);

    // for each partition in the next tree
    dynamic_bitset<> partition(names.size());
    for(int b=0;b<T.size();b++) 
    {
      partition = T[b];

      if (not partition[first])
	partition.flip();

      partition &= mask;

      // Look up record for this partition
      container_t::iterator record = counts.find(partition);
      if (record == counts.end()) {
	counts.insert(container_t::value_type(partition,p_count()));
	record = counts.find(partition);
	assert(record != counts.end());
      }

      // FIXME - we are doing the lookup twice
      p_count& pc = record->second;
      int& C2 = pc.count;
      int C1 = C2;
      if (pc.last_tree != i) {
	pc.last_tree=i;
	C2 += delta;
      }
      
      // add the partition if it wasn't good before, but is now
      if ((C1==0 or C1<min_old) and C2 >= min_new)
	majority.push_back(record);
    }


    // for partition in the majority tree
    for(typeof(majority.begin()) p = majority.begin();p != majority.end();) {
      if ((*p)->second.count < min_new) {
	typeof(p) old = p;
	p++;
	majority.erase(old);
      }
      else
	p++;
    }
  }

  vector<pair<Partition,unsigned> > partitions;
  partitions.reserve( 2*names.size() );
  for(typeof(majority.begin()) p = majority.begin();p != majority.end();p++) {
    const dynamic_bitset<>& partition =(*p)->first;
 
    Partition pi(names,partition,mask);
    unsigned p_count = (*p)->second.count;

    if (valid(pi))
      partitions.push_back(pair<Partition,unsigned>(pi,p_count));
  }

  return partitions;
}


vector<pair<Partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l) 
{
  dynamic_bitset<> mask(sample.names().size());
  mask.flip();
  return get_Ml_partitions_and_counts(sample,l,mask);
}

vector<Partition> 
remove_counts(const vector<pair<Partition,unsigned> >& partitions_and_counts) 
{
  vector<Partition> partitions;
  for(int i=0;i<partitions_and_counts.size();i++)
    partitions.push_back(partitions_and_counts[i].first);

  return partitions;
}


vector<Partition>
get_Ml_partitions(const tree_sample& sample,double l) 
{
  return remove_counts(get_Ml_partitions_and_counts(sample,l));
}

void add_unique(list<dynamic_bitset<> >& masks,const list<dynamic_bitset<> >& old_masks,
		const dynamic_bitset<>& mask) 
{
  // don't add the mask unless contains internal partitions (it could be all 0)
  if (mask.count() < 4) return;

  // don't add the mask if we already have that mask
  foreach(m,masks)
    if (*m == mask) return;

  foreach(m,old_masks)
    if (*m == mask) return;

  // otherwise, add the mask
  masks.push_front(mask);
}


void add_unique(list<dynamic_bitset<> >& masks,const dynamic_bitset<>& mask) 
{
  return add_unique(masks,list<dynamic_bitset<> >(),mask);
}


/// find out which partitions imply the sub-partitions, prefering the most likely.
vector<int> match(const vector<pair<Partition,unsigned> >& full_partitions,
		  const vector<pair<Partition,unsigned> >& sub_partitions)
{
  vector<int> m(sub_partitions.size(),-1);

  for(int i=0;i<sub_partitions.size();i++)
    for(int j=0;j<full_partitions.size();j++) 
    {
      // things that imply us cannot have a higher probability
      if (full_partitions[j].second > sub_partitions[i].second)
	continue;

      // skip this possible parent if it isn't as good as one we've found so far.
      if (m[i] == -1 or full_partitions[j].second > full_partitions[m[i]].second)
      {
	if (implies(full_partitions[j].first,sub_partitions[i].first))
	  m[i] = j;
      }
    }

  return m;
}


// also construct list "who is implied by whom"

// consider only pulling out combinations of branches pointing to the same node

vector<pair<Partition,unsigned> > 
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,const dynamic_bitset<>& mask,
				 double min_rooting,int depth) 
{
  // get list of branches to consider cutting
  //   FIXME - consider 4n-12 most probable partitions, here?
  //         - Perhaps NOT, though.
  vector<Partition> partitions_c50 = get_Ml_partitions(sample, 0.5);
  SequenceTree c50 = get_mf_tree(sample.names(),partitions_c50);
  vector<const_branchview> branches = branches_from_leaves(c50);  

  // construct unit masks
  // - unit masks are masks that come directly from a supported branch (full, or partial)
  list< dynamic_bitset<> > unit_masks;
  for(int b=0;b<branches.size();b++)
    add_unique(unit_masks, mask & branch_partition(c50,branches[b]) );

  // construct beginning masks
  list<dynamic_bitset<> > masks = unit_masks;
  list<dynamic_bitset<> > old_masks = unit_masks;

  // start collecting partitions at M[l]
  vector<pair<Partition,unsigned> > partitions = get_Ml_partitions_and_counts(sample,l,mask);

  // any good mask should be combined w/ other good masks
  list<dynamic_bitset<> > good_masks;
  for(int iterations=0;not masks.empty();iterations++)
  {
    vector<pair<Partition,unsigned> > full_partitions = partitions;

    if (log_verbose) cerr<<"iteration: "<<iterations<<"   depth: "<<depth<<"   masks: "<<masks.size()<<endl;
    list<dynamic_bitset<> > new_good_masks;
    list<dynamic_bitset<> > new_unit_masks;

    // get sub-partitions for each mask
    vector<Partition> all_sub_partitions;
    foreach(m,masks) 
    {
      // get sub-partitions of *m 
      vector<pair<Partition,unsigned> > sub_partitions = get_Ml_partitions_and_counts(sample,l,*m);
    
      // match up sub-partitions and full partitions
      // FIXME - aren't we RE-doing a lot of work, here?
      vector<int> parents = match(full_partitions,sub_partitions);

      // check for partitions with increased support when *m is unplugged
      double rooting=1.0;
      for(int i=0;i<sub_partitions.size();i++) 
      {
	if (not informative(sub_partitions[i].first))
	  continue;
	    
	double r = 1;
	if (parents[i] == -1) {
	  r = (l*sample.size())/double(sub_partitions[i].second);
	}
	else {
	  r = full_partitions[parents[i]].second/double(sub_partitions[i].second);
	  assert(r <= 1.0);
	}

	double OD = statistics::odds(sub_partitions[i].second-5,sample.size(),10);

	// actually, considering bad rooting of low-probability edges may be a better (or alternate)
	// strategy to unplugging edges that are only slightly bad.

	// Determination of rooting probabilities seems to have the largest effect on computation time
	//  - thus, in the long run, new_good_masks has a larger effect than new_unit_masks.
	//  - actually, this makes kind of makes sense...
	//    + new_unit_masks can add partitions they reveal under fairly weak conditions.
	//    + however, unless a new unit mask ends up being a good_mask, it won't trigger the quadratic behavior.

	// What happens when we consider unplugging ratios for branches (now) supported at level l<0.5?
	if (r < min_rooting and OD > 0.5) {
	  add_unique(new_unit_masks,unit_masks,sub_partitions[i].first.group1);
	  add_unique(new_unit_masks,unit_masks,sub_partitions[i].first.group2);
	  rooting = std::min(rooting,r);
	}

	// Store the new sub-partitions we found
	if (r < 0.999 or 
	    (parents[i] != -1 and statistics::odds_ratio(sub_partitions[i].second,
							 full_partitions[parents[i]].second,
							 sample.size(),
							 10) > 1.1)
	    )
	  partitions.push_back(sub_partitions[i]);
      }

      // check if any of our branches make this branch badly rooted
      if (rooting < min_rooting)
	new_good_masks.push_front(*m);
    }

    old_masks.insert(old_masks.end(),masks.begin(),masks.end());
    masks.clear();
    masks = new_unit_masks;

    if (log_verbose) cerr<<"new unit_masks = "<<new_unit_masks.size()<<endl;

    if (depth == 0) continue;

    // FIXME!! We need to find a way to consider only masks which are
    // 'close' togther - defined in terms of the number and support 
    // of branches that are in the between them.

    // fixme - do a convolution here - e.g. 2->1+1 3->1+2 4 ->1+3,2+2
    // otherwise we do 1  - 1,2 - 1,2,3,4 - 1,2,3,4,5,6,7,8
    good_masks.insert(good_masks.end(),new_good_masks.begin(),new_good_masks.end());
    foreach(i,new_good_masks)
      foreach(j,good_masks)
        if (*i != *j)
          add_unique(masks,old_masks,*i & *j);

    // what will we operate on next time? 
    // - perhaps change to look at pairs of branches connected to a node
    // - perhaps depth 3 could be pairs of branches of distance 1
    // - should I use the M[0.5] tree here, or the M[l] tree?
    if (iterations < depth-1) {

      foreach(i,old_masks)
	foreach(j,unit_masks)
	  add_unique(masks,old_masks,*i & *j);

      // old good_masks were considered with unit_masks last_time
      foreach(i,new_good_masks)
      	foreach(j,unit_masks)
      	  add_unique(masks,old_masks,*i & *j);

      // old good_masks were considered with unit_masks already
      foreach(i,old_masks)
	foreach(j,new_good_masks)
	  add_unique(masks,old_masks,*i & *j);
    }

    //cerr<<"   new good masks = "<<new_good_masks.size()<<"    new unit masks = "<<new_unit_masks.size()<<endl;
    //cerr<<"       good masks = "<<good_masks.size()    <<"       total masks = "<<old_masks.size()<<"       found = "<<partitions.size()<<endl;


  }

  return partitions;
}

vector<pair<Partition,unsigned> > 
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,double min_rooting,int depth) 
{
  dynamic_bitset<> mask(sample.names().size());
  mask.flip();
  return get_Ml_sub_partitions_and_counts(sample,l,mask,min_rooting,depth);
  
}

vector<Partition> 
get_Ml_sub_partitions(const tree_sample& sample,double l,double min_rooting,int depth) 
{
  return remove_counts(get_Ml_sub_partitions_and_counts(sample,l,min_rooting,depth));
}


// there are actually a lot of things we might want to do
// * skip certain lines (e.g. in front)
// * stop at a certain line
// * report no more than X trees (compress the trees)
// * 

struct string_to_standardized_tree_op: public accumulator<string>
{
  accumulator<SequenceTree>& tree_op;

  void operator()(const std::string& line);

  string_to_standardized_tree_op(accumulator<SequenceTree>& o):tree_op(o) {};
};

void string_to_standardized_tree_op::operator()(const string& line)
{
  try {
    SequenceTree T = standardized(line);
    tree_op(T);
  }
  catch (std::exception& e) {
    std::cerr<<"Badly formed tree (ignored): "<<e.what()<<endl;
  }
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
