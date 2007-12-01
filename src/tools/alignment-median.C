#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <list>
#include <numeric>
#include "myexception.H"
#include "alignment.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment-util.H"
#include "distance-methods.H"

#include <boost/program_options.hpp>
#include <boost/numeric/ublas/matrix.hpp>

namespace ublas = boost::numeric::ublas;
namespace po = boost::program_options;
using po::variables_map;

// FIXME - also show which COLUMNS are more that 99% conserved?

// Questions: 1. where does these fit on the length distribution:
//                 a) pair-median  b) splits-median c) MAP
//            2. where do the above alignments fit on the L/prior/L+prior distribution?
//            3. graph average distance between alignments in the (0,q)-th quantile.
//            4. graph autocorrelation to see how quickly it decays...
//            5. distance between the two medians, and the MAP...
//            6. E (average distance) and Var (average distance)

using namespace std;

void do_setup(const variables_map& args,vector<alignment>& alignments) 
{
  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();
  unsigned skip = args["skip"].as<unsigned>();

  // --------------------- try ---------------------- //
  cerr<<"Loading alignments...";
  list<alignment> As = load_alignments(cin,load_alphabets(args),skip,maxalignments);
  alignments.insert(alignments.begin(),As.begin(),As.end());
  cerr<<"done. ("<<alignments.size()<<" alignments)"<<endl;
  if (not alignments.size())
    throw myexception()<<"Alignment sample is empty.";
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "Produce help message")
    ("skip",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("metric", value<string>()->default_value("splits"),"type of distance: pairs, splits, splits2")
    ("analysis", value<string>()->default_value("matrix"), "Analysis: matrix, median, diameter")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-median [OPTIONS] < in-file\n";
    cout<<"Don't use this program.  It doesn't work.\n";
    cout<<"Find the 'median' alignment in a list of alignments.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

typedef long int (*distance_fn)(const ublas::matrix<int>& ,const vector< vector<int> >&,const ublas::matrix<int>& ,const vector< vector<int> >&);

ublas::matrix<double> distances(const vector<ublas::matrix<int> >& Ms,
				const vector< vector< vector<int> > >& column_indexes,
				distance_fn distance)
{
  assert(Ms.size() == column_indexes.size());
  ublas::matrix<double> D(Ms.size(),Ms.size());

  for(int i=0;i<D.size1();i++) 
    for(int j=0;j<D.size2();j++)
      D(i,j) = distance(Ms[i],column_indexes[i],
			Ms[j],column_indexes[j]);
  return D;
}

double diameter(const ublas::matrix<double>& D)
{
  double total = 0;
  for(int i=0;i<D.size1();i++)
    for(int j=0;j<i;j++)
      total += D(i,j);
  
  int N = D.size1() * (D.size1() - 1) /2;

  return total/N;
}


int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line ---------//
    variables_map args = parse_cmd_line(argc,argv);

    string analysis = args["analysis"].as<string>();

    string metric = args["metric"].as<string>();

    //------------ Load alignment and tree ----------//
    vector<alignment> alignments;
    vector<ublas::matrix<int> > Ms;

    do_setup(args,alignments);
    for(int i=0;i<alignments.size();i++)
      alignments[i] = chop_internal(alignments[i]);

    if (alignments.size() > 1) {
      int n = alignments[0].n_sequences();
      assert(alignments[1].n_sequences() == n);
      assert(alignments[1].seqlength(n-1) == alignments[0].seqlength(n-1));
    }

    //--------- Construct alignment indexes ---------//
    for(int i=0;i<alignments.size();i++)
      Ms.push_back(M(alignments[i]));

    vector< vector< vector<int> > >  column_indexes;
    for(int i = 0;i<alignments.size();i++)
      column_indexes.push_back( column_lookup(alignments[i]) );

    //--------- Determine distance function -------- //
    distance_fn distance;

    distance = splits_distance;
    if (metric == "splits2")
      distance = splits_distance2;
    else if (metric == "pairwise")
      distance = pairs_distance;
      
    //---------- write out distance matrix --------- //
    if (analysis == "matrix") 
    {
      ublas::matrix<double> D = distances(Ms,column_indexes,distance);

      for(int i=0;i<D.size1();i++) {
	vector<double> v(D.size2());
	for(int j=0;j<v.size();j++)
	  v[j] = D(i,j);
	cout<<join(v,'\t')<<endl;
      }

      exit(0);
    }
    else if (analysis == "median") 
    {
      ublas::matrix<double> D = distances(Ms,column_indexes,distance);

      //----------- accumulate distances ------------- //
      vector<double> ave_distances( Ms.size() , 0);
      for(int i=0;i<ave_distances.size();i++)
	for(int j=0;j<i;j++) {
	  ave_distances[i] += D(i,j);
	  ave_distances[j] += D(i,j);
	}
      for(int i=0;i<ave_distances.size();i++)
	ave_distances[i] /= (D.size1()-1);

      int argmin = ::argmin(ave_distances);

      cout<<alignments[argmin]<<endl;

      // Get a list of alignments in decreasing order of E D(i,A)
      vector<int> items = iota<int>(Ms.size());
      sort(items.begin(),items.end(),sequence_order<double>(ave_distances));

      cerr<<endl;
      for(int i=0;i<Ms.size() and i < 5;i++) 
      {
	int j = items[i];
	cerr<<"alignment = "<<i<<"   length = "<<Ms[j].size1();
	cerr<<"   E D = "<<ave_distances[j]<<endl;
      }

      cerr<<endl;
      double total=0;
      for(int i=1;i<items.size() and i < 5;i++) {
	for(int j=0;j<i;j++)
	  total += D(items[i], items[j]);
	
	cerr<<"fraction = "<<double(i)/(items.size()-1)<<"     AveD = "<<double(total)/(i*i+i)*2<<endl;
      }
      cerr<<endl;
      cerr<<"diameter = "<<diameter(D)<<endl;
      exit(0);  
    }
    else if (analysis == "diameter")
    {
      ublas::matrix<double> D = distances(Ms,column_indexes,distance);

      cout<<"diameter = "<<diameter(D)<<endl;
    }
    
    ublas::matrix<double> D = distances(Ms,column_indexes,distance);

    //----------- accumulate distances ------------- //
    vector<double> ave_distances( Ms.size() , 0);
    for(int i=0;i<ave_distances.size();i++)
      for(int j=0;j<i;j++) {
	ave_distances[i] += D(i,j);
	ave_distances[j] += D(i,j);
      }
    for(int i=0;i<ave_distances.size();i++)
      ave_distances[i] /= (D.size1()-1);
    
    int argmin = ::argmin(ave_distances);

    // Get a list of alignments in decreasing order of E D(i,A)
    vector<int> items = iota<int>(Ms.size());
    sort(items.begin(),items.end(),sequence_order<double>(ave_distances));

    for(int i=0;i<Ms.size();i++) 
    {
      int j = items[i];
      cerr<<"alignment = "<<i<<"   length = "<<Ms[j].size1();
      cerr<<"   E D = "<<ave_distances[j]
	       <<"   E D1 = "<<ave_distances[argmin];
      cerr<<endl;
    }


    double total=0;
    for(int i=1;i<items.size();i++) {
      for(int j=0;j<i;j++)
	total += D(items[i], items[j]);

      cerr<<"fraction = "<<double(i)/(items.size()-1)<<"     AveD = "<<double(total)/(i*i+i)*2<<endl;
    }
    
  }
  catch (exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
