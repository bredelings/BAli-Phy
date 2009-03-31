#include "index-matrix.H"
#include <utility>
#include <fstream>
#include <string>
#include <cmath>
#include <list>
#include <numeric>
#include "myexception.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment-util.H"
#include "distance-methods.H"
#include "rng.H"
#include "statistics.H"

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>

using namespace boost;

typedef adjacency_list< vecS, vecS, bidirectionalS> Graph; 
typedef graph_traits<Graph>::vertex_descriptor Vertex;
typedef graph_traits<Graph>::edge_descriptor Edge_t;

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

void do_setup(const variables_map& args,vector<alignment>& alignments) 
{
  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();
  unsigned skip = args["skip"].as<unsigned>();

  // --------------------- try ---------------------- //
  if (log_verbose) std::cerr<<"alignment-max: Loading alignments...";
  list<alignment> As = load_alignments(std::cin,load_alphabets(args),skip,maxalignments);
  alignments.insert(alignments.begin(),As.begin(),As.end());
  if (log_verbose) std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
  if (not alignments.size())
    throw myexception()<<"Alignment sample is empty.";
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ("skip",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("analysis",value<string>()->default_value("wsum"),"sum, wsum, multiply")
    ("out",value<string>()->default_value("-"),"Output file (defaults to stdout)")
    ("out-probabilities",value<string>(),"Output file for column probabilities, if specified")
    ("verbose","Output more log messages on stderr.")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-max [OPTIONS] < alignments-file\n";
    cout<<"Construct a posterior decoding alignment to summarize an alignment sample.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}


/// Extract column @c from the index-matrix @M
vector<int> get_column(const ublas::matrix<int>& M, int c)
{
  vector<int> column(M.size2(),-1);

  for(int i=0;i<column.size();i++)
    if (M(c,i) >= 0)
      column[i] = M(c,i);

  return column;
}

/// How many letters (e.g. non-gap) are there in column @v?
unsigned n_letters(const vector<int>& v)
{
  unsigned n=0;
  for(int i=0;i<v.size();i++)
    if (v[i]>=0)
      n++;

  return n;
}

/// Augments a @column of residues with what has been @emitted so far.
struct emitted_column
{
  /// How many letters have been emitted in each sequence, after this column?
  vector<int> emitted;

  /// What are the indices of letters aligned in this column?
  vector<int> column;

  /// How many sequences are there?
  unsigned size() const {return emitted.size();}

  emitted_column(const vector<int>& e,const vector<int>& c)
    :emitted(e),column(c)
  { 
    assert(emitted.size() == column.size());
  }

  emitted_column(int n)
    :emitted(n,-1),column(n,-1)
  {}
};


/// Gives -1:0:1 if the relationship of @c1 and @c2 is <:=:>
int get_column_order(const vector<int>& c1, const vector<int>& c2)
{
  if (c1.size() != c2.size()) {
    throw myexception()<<"vector sizes are different!";
    assert(c1.size() == c2.size());
  }

  for(int i=0;i<c1.size();i++) {
    if (c1[i] < c2[i])
      return -1;
    else if (c1[i] > c2[i])
      return 1;
  }

  return 0;
}

/// An ordering class for column indices
struct column_order
{
  bool operator()(const vector<int>& c1, const vector<int>& c2) const
  {
    return (get_column_order(c1,c2) == -1);
  }
};

/// An ordering class for emitted_column structures
struct emitted_column_order
{
  bool operator()(const emitted_column& c1,const emitted_column& c2) const
  {
    int o = get_column_order(c1.emitted, c2.emitted);

    if (o == -1)
      return true;
    else if (o== 1)
      return false;

    o = get_column_order(c1.column, c2.column);
    
    return (o == -1);
  }
};

int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //------------ Load alignment and tree ----------//
    vector<alignment> alignments;
    vector<ublas::matrix<int> > Ms;

    do_setup(args,alignments);
    for(int i=0;i<alignments.size();i++)
      alignments[i] = chop_internal(alignments[i]);

    if (not alignments.size())
      throw myexception()<<"Didn't read any alignments!";      

    int N = alignments[0].n_sequences();
    if (alignments.size() > 1) {
      assert(alignments[1].n_sequences() == N);
      assert(alignments[1].seqlength(N-1) == alignments[0].seqlength(N-1));
    }
    vector<int> L(N);
    for(int i=0;i<L.size();i++)
      L[i] = alignments[0].seqlength(i);
    
    //--------- Construct alignment indexes ---------//
    for(int i=0;i<alignments.size();i++)
      Ms.push_back(M(alignments[i]));

    // map emitted columns -> x
    typedef map<emitted_column,int,emitted_column_order> emitted_column_map;
    emitted_column_map emitted_columns;

    // map bare columns    -> y
    typedef map< vector<int>, int, column_order> column_map;
    column_map columns;

    // map x -> y
    vector<int> emitted_to_bare;

    // how many times did we see each bare column?
    vector<int> counts;

    Graph g;
    Vertex S = add_vertex(g); // add the start node
    emitted_to_bare.push_back(-1);
    Vertex E = add_vertex(g); // add the end node
    emitted_to_bare.push_back(-1);

    for(int i=0;i<Ms.size();i++)
    {
      // prev = S
      vector<int> emitted(N,-1);

      emitted_column C(N);

      int x_current = get(vertex_index,g, S);

      for(int c=0;c<Ms[i].size1();c++)
      {
	C.column = get_column(Ms[i],c);
	if (not n_letters(C.column))
	  continue;

	// Check that sequential columns don't jump
	for(int i=0;i<C.size();i++)
	  if (C.column[i] >= 0) {
	    assert(C.column[i] == C.emitted[i]+1);
	    C.emitted[i] = C.column[i];
	  }

	// Look up the column, creating a new index if necessary
	emitted_column_map::iterator x_record = emitted_columns.find(C);

	if (x_record == emitted_columns.end()) 
	{
	  Vertex v = add_vertex(g);
	  int vi = get(vertex_index,g,v);
	  assert(vi == emitted_columns.size()+2);
	  emitted_columns.insert(emitted_column_map::value_type(C,vi));
	  x_record = emitted_columns.find(C);
	  assert(x_record != emitted_columns.end());

	  column_map::iterator y_record = columns.find(C.column);
	  if (y_record == columns.end()) 
	  {
	    columns.insert(column_map::value_type(C.column, columns.size()));
	    y_record = columns.find(C.column);
	    assert(y_record != columns.end());

	    counts.push_back(0);
	    assert(counts.size() == columns.size());
	  }

	  emitted_to_bare.push_back(y_record->second);
	  assert(emitted_to_bare.size()-1 == vi);

	  assert(emitted_columns.size()+2 == emitted_to_bare.size());
	}

	int x_prev = x_current;
	x_current = x_record->second;

	// Increment column count
	++counts[emitted_to_bare[x_current]];

	// Record edge prev->current
	{
	  Vertex v1 = vertex(x_prev, g);
	  Vertex v2 = vertex(x_current, g);
	  ::add_edge(v1,v2,g);
	}
      }
      // add edge to end
      {
	Vertex v = vertex(x_current,g);
	add_edge(v,E,g);
      }
    }
    emitted_column_order eco;


    const int n_vertices = emitted_to_bare.size();
    vector<emitted_column_map::iterator> ec_from_x(n_vertices,emitted_columns.end());
    foreach(ec,emitted_columns)
    {
      ec_from_x[ec->second] = ec;
    }

    if (log_verbose) cerr<<"\nalignment-max: checking edges...\n";
    {
      graph_traits<Graph>::vertex_iterator vi, vi_end;
      for (tie(vi, vi_end) = ::vertices(g); vi != vi_end; ++vi) 
	{
	  int index1 = get(vertex_index,g,*vi);
	  graph_traits<Graph>::out_edge_iterator ei, eend;
	  for(tie(ei,eend) = out_edges(*vi,g); ei != eend; ++ei)
	    { 
	      int index2 = get(vertex_index,g,target(*ei,g));

	      if (index1 >= ec_from_x.size() or index1 < 0)
		throw myexception()<<"trouble...";
	      if (index2 >= ec_from_x.size() or index2 < 0)
		throw myexception()<<"trouble...";
	      emitted_column_map::iterator ec1 = ec_from_x[index1];
	      emitted_column_map::iterator ec2 = ec_from_x[index2];

	      if (index1 == 0 or index2 == 1) continue;

	      if (ec1->first.size() != ec2->first.size() and log_verbose >=2) {
		cerr<<"alignment-max: index1 = "<<index1<<endl;
		cerr<<"alignment-max: index2 = "<<index2<<endl;
		cerr<<"alignment-max: ec1->first.size() = "<<ec1->first.size()<<endl;
		cerr<<"alignment-max: ec2->first.size() = "<<ec2->first.size()<<endl;
	      }
		

	      if (not eco(ec1->first,ec2->first) and log_verbose >=2)
	      {
		cerr<<"alignment-max: ";
		for(int i=0;i<ec1->first.size();i++)
		  cerr<<ec1->first.emitted[i]<<" ";
		cerr<<endl;
		cerr<<"alignment-max: ";
		for(int i=0;i<ec1->first.size();i++)
		  cerr<<ec1->first.column[i]<<" ";
		cerr<<endl;
		cerr<<endl;
		cerr<<"alignment-max: ";
		for(int i=0;i<ec2->first.size();i++)
		  cerr<<ec2->first.emitted[i]<<" ";
		cerr<<endl;
		cerr<<"alignment-max: ";
		for(int i=0;i<ec2->first.size();i++)
		  cerr<<ec2->first.column[i]<<" ";
		  cerr<<endl;
	      } 

	    }
	}
    }
    if (log_verbose) cerr<<"alignment-max: done."<<endl;

    //---------- Construct score ------------------//

    int type = -1;
    string analysis = args["analysis"].as<string>();
    if (analysis == "sum")
      type = 0;
    else if (analysis == "wsum")
      type = 1;
    else if (analysis == "multiply")
      type = 2;
    else
      throw myexception()<<"I don't recognize analysis type '"<<analysis<<"'.";

    vector<double> score(counts.size());
    foreach(c,columns)
    {
      int n = n_letters(c->first);
      assert(n > 0);

      int i = c->second;

      score[i] = double(counts[i])/Ms.size();
      if (type == 1)
	score[i] *= n;
      else if (type == 2)
	score[i] = log(score[i]);
    }


    //----------------- Forward Sums -------------------//

    vector<Vertex> sorted_vertices;
    topological_sort(g, std::back_inserter(sorted_vertices));
    std::reverse(sorted_vertices.begin(), sorted_vertices.end());

    vector<int> sorted_indices(sorted_vertices.size());
    for(int i=0;i<sorted_indices.size();i++)
      sorted_indices[i] = get(vertex_index,g,sorted_vertices[i]);
    assert(sorted_indices[0] == 0);
    assert(sorted_indices.back() == 1);

    vector<double> forward(n_vertices, -1);
    vector<int> visited(n_vertices, 0);
    vector<int> from(n_vertices, -1);
    forward[0] = 0;
    visited[0] = 1;

    for(int i=1;i<n_vertices;i++)
    {
      int v2i = sorted_indices[i];
      assert(not visited[v2i]);

      int v2 = vertex(v2i, g);

      double best = 0;
      int argmax = -1;
      graph_traits<Graph>::in_edge_iterator e, end;
      for(tie(e,end) = in_edges(v2,g); e != end; ++e)
      { 
	Vertex v1 = source(*e,g);
	int v1i = get(vertex_index,g,v1);
	assert(visited[v1i]);

	if (argmax == -1) {
	  best = forward[v1i];
	  argmax = v1i;
	}
	else {
	  if (forward[v1i] > best) {
	    argmax = v1i;
	    best = forward[v1i];
	  }
	}
      }
      assert(argmax != -1);
      from[v2i] = argmax;
      
      forward[v2i] = best;
      if (emitted_to_bare[v2i] != -1)
	forward[v2i] += score[emitted_to_bare[v2i]];
      
      visited[v2i] = 1;
    }

    assert(visited[1]);

    if (log_verbose) cerr<<"alignment-max: Best score is: "<<forward[1]<<endl;

    //----------------- Backward Path Selection -------------------//
    vector<int> path(1,1); // start with just the end state

    while(path.back() != 0) {
      int S = from[path.back()];
      path.push_back(S);
    }
    std::reverse(path.begin(),path.end());

    //---------------- Create alignment matrix -------------------//
    ublas::matrix<int> M(path.size()-2,N);

    for(int i=0;i<M.size1();i++) {
      int S = path[i+1];
      emitted_column_map::iterator ec = ec_from_x[S];
      for(int j=0;j<N;j++)
	M(i,j) = (ec->first).column[j];
    }

    alignment amax = get_alignment(M,alignments[0]);

    //-------------------- Write output -------------------------//
    string out = args["out"].as<string>();

    if (out == "-")
      cout<<amax<<endl;
    else {
      ofstream outfile(out.c_str());
      if (not outfile)
	throw myexception()<<"Can't open '"<<outfile<<"' to write result!";

      outfile<<amax<<endl;
      outfile.close();
    }

    if (args.count("out-probabilities")) {
      string outp = args["out-probabilities"].as<string>();
      ofstream outfile(outp.c_str());
      if (not outfile)
	throw myexception()<<"Can't open '"<<outfile<<"' to write column probabilities!";
      for(int i=1;i<path.size()-1;i++)
      {
	int c = counts[emitted_to_bare[path[i]]];
	outfile<<double(c)/Ms.size()<<endl;
      }      
      outfile.close();
    }
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-max: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
