/*
   Copyright (C) 2008-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include "alignment/index-matrix.H"
#include <utility>
#include <fstream>
#include <string>
#include <cmath>
#include <list>
#include <numeric>
#include "myexception.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment/alignment-util.H"
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
    ("help,h", "Produce help message")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ("skip,s",value<unsigned>()->default_value(0),"Number of alignment samples to skip")
    ("max-alignments,m",value<int>()->default_value(1000),"Maximum number of alignments to analyze")
    ("analysis",value<string>()->default_value("wsum"),"sum, wsum, multiply")
    ("out,o",value<string>()->default_value("-"),"Output file (defaults to stdout)")
    ("out-probabilities,p",value<string>(),"Output file for column probabilities, if specified")
    ("verbose,v","Output more log messages on stderr.")
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
vector<int> get_column(const matrix<int>& M, int c)
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

  bool character(int i) const {return column[i] >= 0;}

  emitted_column(const vector<int>& e,const vector<int>& c)
    :emitted(e),column(c)
  { 
    assert(emitted.size() == column.size());
  }

  emitted_column(int n)
    :emitted(n,0),column(n,-1)
  {}
};

/// How many characters must be emitted by any column that comes immediately before c ?
vector<int> emitted_before(const emitted_column& c)
{
  vector<int> emitted = c.emitted;
  for(int i=0;i<c.size();i++)
    if (c.character(i))
      emitted[i]--;

  return emitted;
}

bool get_emitted_column(emitted_column& C,const matrix<int>& m, int c)
{
  // the "emitted" value carries over from the previous iteration.
  C.column = get_column(m,c);

  // skip empty columns
  if (not n_letters(C.column))
    return false;
  
  // Update the "emitted" value of C
  // Check that sequential columns don't jump by more than one emitted character
  for(int i=0;i<C.size();i++)
    if (C.character(i)) {
      assert(C.column[i] == C.emitted[i]);
      C.emitted[i] = C.column[i]+1;
    }

  return true;
}


/// Gives -1:0:1 if the relationship of @c1 and @c2 is <:=:>
int get_column_order(const vector<int>& c1, const vector<int>& c2)
{
  if (c1.size() != c2.size()) {
    throw myexception()<<"comparing 2 columns: different sizes!";
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

typedef map<emitted_column,int, emitted_column_order> emitted_column_map;
typedef map< vector<int>, int, column_order> column_map;
typedef map< vector<int>, vector<int>, column_order> emitted_map;

struct MPD
{
  const int N;

  vector<int> L;

  alignment A0;

  int n_samples;

  Graph g;
  Vertex vertex_start;
  Vertex vertex_end;

  /// map emitted columns -> x
  emitted_column_map emitted_columns;

  /// map bare columns    -> y
  column_map columns;

  /// map x -> y
  vector<int> emitted_to_bare;

  /// how many times did we see each bare column?
  vector<int> counts;

  /// emitted before column -> {x1...xn}
  emitted_map before;

  /// emitted after column -> {x1...xn}
  emitted_map after;

  /// map x -> &(C,x).  This is only valid after calling get_best_path( ), and before add_alignment( )
  vector<emitted_column_map::iterator> ec_from_x;

  void check_edges_go_forwards_only() const;

  emitted_column_map::iterator create_new_emitted_column(const emitted_column& C);

  void add_emitted_column(const emitted_column& C);

  void add_alignment(const alignment& A);

  vector<double> get_score(int) const;

  vector<int> get_best_path(const vector<double>& score);
  
  alignment get_best_alignment(int);
  
  int n_vertices() const {return emitted_to_bare.size();}

  vector<double> get_column_probabilities(const alignment&) const;

  MPD(const alignment&);
};


MPD::MPD(const alignment& A)
  :N( A.n_sequences() ), L( N ), A0(A), n_samples( 0 )
{
  //------------ Determine sequence lengths ----------//
  for(int i=0;i<L.size();i++)
    L[i] = A.seqlength(i);
    
  vertex_start = add_vertex(g); // add the start node
  emitted_to_bare.push_back(-1); // the start node doesn't correspond to a column
  int x_start = get(vertex_index, g, vertex_start);

  vector<int> nothing_emitted(N,0);
  // after x_start, nothing has been emitted
  after[nothing_emitted].push_back(x_start);

  vertex_end = add_vertex(g); // add the end node
  emitted_to_bare.push_back(-1); // the start node doesn't correspond to a column
  int x_end = get(vertex_index, g, vertex_end);

  vector<int> everything_emitted = L;
  // before x_end, everything has been emitted
  before[everything_emitted].push_back(x_end);
}

void MPD::check_edges_go_forwards_only() const
{
  emitted_column_order eco;

  graph_traits<Graph>::vertex_iterator vi, vi_end;
  for (tie(vi, vi_end) = ::vertices(g); vi != vi_end; ++vi) 
  {
    int index1 = get(vertex_index,g,*vi);
    graph_traits<Graph>::out_edge_iterator ei, eend;
    for(tie(ei,eend) = out_edges(*vi,g); ei != eend; ++ei)
    { 
      int index2 = get(vertex_index,g,target(*ei,g));
      
      if (index1 >= ec_from_x.size() or index1 < 0)
	throw myexception()<<"Out of bounds...";
      if (index2 >= ec_from_x.size() or index2 < 0)
	throw myexception()<<"Out of bounds...";
      emitted_column_map::iterator ec1 = ec_from_x[index1];
      emitted_column_map::iterator ec2 = ec_from_x[index2];
      
      // skip the Start and End states
      if (index1 == 0 or index2 == 1) continue;
      
      // check that the columns all have the same size
      if (ec1->first.size() != ec2->first.size() and log_verbose >=2) {
	cerr<<"alignment-max: index1 = "<<index1<<endl;
	cerr<<"alignment-max: index2 = "<<index2<<endl;
	cerr<<"alignment-max: ec1->first.size() = "<<ec1->first.size()<<endl;
	cerr<<"alignment-max: ec2->first.size() = "<<ec2->first.size()<<endl;
      }
      
      // check that if ec1 -> ec2, then also ec1 < ec2
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

emitted_column_map::iterator 
MPD::create_new_emitted_column(const emitted_column& C)
{
  Vertex v = add_vertex(g);
  int vi = get(vertex_index,g,v);
  assert(vi == emitted_columns.size()+2);

  // Add the mapping from C -> vi to the emitted_column map
  emitted_columns.insert(emitted_column_map::value_type(C,vi));
  emitted_column_map::iterator x_record = emitted_columns.find(C);
  assert(x_record != emitted_columns.end());

  // if this bare column has not been seen before
  column_map::iterator y_record = columns.find(C.column);
  if (y_record == columns.end()) 
  {
    // Add the mapping from C.column -> columns.size() to the bare column map
    columns.insert(column_map::value_type(C.column, columns.size()));
    y_record = columns.find(C.column);
    assert(y_record != columns.end());
    
    // This new bare column has no counts
    counts.push_back(0);
    assert(counts.size() == columns.size());
  }
  
  // map the emitted_column index (emitted.size()) to the bare column index (y_record->second)
  emitted_to_bare.push_back(y_record->second);
  assert(emitted_to_bare.size()-1 == vi);
  
  assert(emitted_columns.size()+2 == emitted_to_bare.size());

  return x_record;
}

void
MPD::add_emitted_column(const emitted_column& C)
{
  int x_current = -1;

  // Look up the column, creating a new index if necessary
  emitted_column_map::iterator x_record = emitted_columns.find(C);

  // if this emitted column has not been seen before
  if (x_record == emitted_columns.end()) 
  {
    x_record = create_new_emitted_column(C);
    int vi = x_record->second;
    
    // If this emitted column is new, then add edges to it.
    vector<int> e_before = emitted_before(C);
    vector<int> e_after = C.emitted;
    before[e_before].push_back(vi);
    after[e_after].push_back(vi);
    
    // Find the vertex index for the current column
    x_current = x_record->second;
    
    // Add edges TO this vertex
    const vector<int>& prev_vertex_indices = after[e_before];
    assert(prev_vertex_indices.size());
    foreach(x_prev,prev_vertex_indices)
    {
      Vertex v1 = vertex(*x_prev, g);
      Vertex v2 = vertex(x_current, g);
      ::add_edge(v1, v2, g);
    }
    
    // Add edges FROM this vertex
    const vector<int>& next_vertex_indices = before[e_after];
    //  assert(next_vertex_indices.size());
    foreach(x_next, next_vertex_indices)
    {
      Vertex v1 = vertex(x_current, g);
      Vertex v2 = vertex(*x_next, g);
      ::add_edge(v1, v2, g);
    }
  }
  
  x_current = x_record->second;
  
  // Increment column count
  ++counts[emitted_to_bare[x_current]];
}

void MPD::add_alignment(const alignment& A)
{
  check_same_sequence_lengths(L, A);

  emitted_column C(N);

  matrix<int> m = M(A);

  for(int c=0; c<m.size1(); c++)
  {
    // the "emitted" value carries over from the previous iteration.
    if (not get_emitted_column(C, m, c)) continue;
    
    add_emitted_column(C);
  }

  n_samples++;
}

vector<double> MPD::get_score(int type) const
{
  vector<double> score(counts.size());

  foreach(c, columns)
  {
    int n = n_letters(c->first);
    assert(n > 0);
    
    int i = c->second;
    
    score[i] = double(counts[i])/n_samples;
    if (type == 1)
      score[i] *= n;
    else if (type == 2)
      score[i] = log(score[i]);
  }

  return score;
}

vector<int>
MPD::get_best_path(const vector<double>& score)
{
  //----------------- construct a map from index -> &(EC,index) ---------------//
  ec_from_x = vector<emitted_column_map::iterator>(n_vertices(), emitted_columns.end());
  foreach(ec, emitted_columns)
    ec_from_x[ec->second] = ec;
  
  if (log_verbose) {
    cerr<<"\nalignment-max: checking edges...\n";
    check_edges_go_forwards_only();
    cerr<<"alignment-max: done."<<endl;
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

  vector<double> forward(n_vertices(), -1);
  vector<int> visited(n_vertices(), 0);
  vector<int> from(n_vertices(), -1);
  forward[0] = 0;
  visited[0] = 1;

  for(int i=1;i<n_vertices();i++)
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

  return path;
}

alignment MPD::get_best_alignment(int type)
{
  vector<double> score = get_score(type);
  
  //----------------- Forward Sums -------------------//
  
  vector<int> path = get_best_path(score);
  
  //---------------- Create alignment matrix -------------------//
  matrix<int> M(path.size()-2, L.size());
  
  for(int i=0;i<M.size1();i++) {
    int S = path[i+1];
    emitted_column_map::iterator ec = ec_from_x[S];
    for(int j=0;j<L.size();j++)
      M(i,j) = (ec->first).column[j];
  }
  
  alignment amax = get_alignment(M, A0);
  return amax;
}

vector<double> MPD::get_column_probabilities(const alignment& A) const
{
  vector<double> column_pr(A.length(), 0);
      
  matrix<int> m = ::M(A);

  for(int c=0; c<A.length(); c++)
  {
    vector<int> column = get_column(m, c);
    column_map::const_iterator y_record = columns.find(column);

    if (y_record != columns.end())
    {
      int y_index = y_record->second;
      int count = counts[y_index];
      column_pr[c] = double(count)/n_samples;
    }
  }

  return column_pr;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

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

    //------------ Load alignment and tree ----------//
    vector<alignment> alignments;

    do_setup(args,alignments);
    for(int i=0;i<alignments.size();i++)
      alignments[i] = chop_internal(alignments[i]);

    if (not alignments.size())
      throw myexception()<<"Didn't read any alignments!";      

    //--------- Construct alignment indexes ---------//
    MPD mpd( alignments[0] );

    for(int i=0;i<alignments.size();i++)
      mpd.add_alignment( alignments[i] );

    alignment amax = mpd.get_best_alignment( type );
    amax = get_ordered_alignment(amax);

    //------------------ Write best alignment -------------------//
    string out = args["out"].as<string>();

    if (out == "-")
      cout<<amax<<endl;
    else {
      ofstream outfile(out.c_str());
      if (not outfile)
	throw myexception()<<"Can't open '"<<out<<"' to write result!";

      outfile<<amax<<endl;
      outfile.close();
    }

    //------------------ Write column probabilites -------------------//
    if (args.count("out-probabilities")) 
    {
      vector<double> column_probabilities = mpd.get_column_probabilities( amax );

      string outp = args["out-probabilities"].as<string>();
      ofstream outfile(outp.c_str());

      if (not outfile)
	throw myexception()<<"Can't open '"<<out<<"' to write column probabilities!";

      for(int c=0; c < column_probabilities.size(); c++)
	outfile<<column_probabilities[c]<<endl;

      outfile.close();
    }
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-max: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
