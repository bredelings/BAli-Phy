#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <list>
#include <numeric>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "setup.H"
#include "alignmentutil.H"
#include "alignment-util.H"
#include "distance-methods.H"
#include "rng.H"

// FIXME - also show which COLUMNS are more that 99% conserved?

// Questions: 1. where does these fit on the length distribution:
//                 a) pair-median  b) splits-median c) MAP
//            2. where do the above alignments fit on the L/prior/L+prior distribution?
//            3. graph average distance between alignments in the (0,q)-th quantile.
//            4. graph autocorrelation to see how quickly it decays...
//            5. distance between the two medians, and the MAP...
//            6. E (average distance) and Var (average distance)

using namespace std;


void do_setup(Arguments& args,vector<alignment>& alignments) 
{
  // ------------- Try to load alignments ------------ //
  int maxalignments = args.loadvalue("maxalignments",1000);

  string tag = "align[sample";
  if (args.set("tag"))
    tag = args["tag"];

  // --------------- Alphabets to try --------------- //
  vector<OwnedPointer<alphabet> > alphabets;
  alphabets.push_back(DNA());
  alphabets.push_back(RNA());
  alphabets.push_back(AminoAcids());

  // --------------------- try ---------------------- //
  std::cerr<<"Loading alignments...";
  list<alignment> As = load_alignments(std::cin,tag,alphabets,maxalignments);
  alignments.insert(alignments.begin(),As.begin(),As.end());
  std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
}


int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cerr);

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.set("seed")) {
      seed = convertTo<unsigned long>(args["seed"]);
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    //    cout<<"random seed = "<<seed<<endl<<endl;
    
    //------------ Load alignment and tree ----------//
    vector<alignment> alignments;
    vector<ublas::matrix<int> > Ms;

    do_setup(args,alignments);
    for(int i=0;i<alignments.size();i++)
      alignments[i] = chop_internal(alignments[i]);

    if (alignments.size() > 1) {
      int n = alignments[0].size2();
      assert(alignments[1].size2() == n);
      assert(alignments[1].seqlength(n-1) == alignments[0].seqlength(n-1));
    }

    //--------- Construct alignment indexes ---------//
    for(int i=0;i<alignments.size();i++)
      Ms.push_back(M(alignments[i]));

    vector< vector< vector<int> > >  column_indexes;
    for(int i = 0;i<alignments.size();i++)
      column_indexes.push_back( column_lookup(alignments[i]) );

    //--------- Determine distance function -------- //

    long int (*distance)(const ublas::matrix<int>& ,const vector< vector<int> >&,
				const ublas::matrix<int>& ,const vector< vector<int> >&);

    distance = pairs_distance;
    if (args.set("type") and args["type"] == "splits")
      distance = splits_distance;

    //---------------- pick an order --------------- //
    vector<int> order(Ms.size());
    for(int i=0;i<order.size();i++)
      order[i] = i;
    order = randomize(order);
    vector<int> iorder = invert(order);

    //----------- accumulate distances ------------- //
    vector< vector<double> > distances(Ms.size());
    vector<double> total1;
    vector<double> total2(Ms.size(),0);

    for(int i=0;i<Ms.size();i++) {

      int o = order[i];
      distances[o].resize(Ms.size());

      total1.push_back(0);
      for(int j=0;j<Ms.size();j++) {
	
	double D = distance(Ms[o],column_indexes[o],
			    Ms[j],column_indexes[j]);

	distances[o][j] = D;

	total1[i] += distances[o][j];
	total2[j] += distances[o][j];
      }

      int argmin1 = argmin(total1);
      int argmin2 = argmin(total2);

      std::cerr<<"alignment = "<<i<<"   length = "<<Ms[i].size1();
      std::cerr<<"   E D = "<<total1[i]/Ms.size()
	       <<"   E D1 = "<<total1[argmin1]/Ms.size()
	       <<"   E D2 ~ "<<total2[argmin2]/(i+1);
      std::cerr<<"    D("<<order[argmin1]<<","<<argmin2<<") = "<<distances[order[argmin1]][argmin2]<<std::endl;
    }
    assert(total1.size() == total2.size());
    assert(argmin(total1) == argmin(total2));

    std::cout<<alignments[iorder[argmin(total1)]]<<std::endl;

    // print radius vs fraction
    vector<int> order2(Ms.size());
    for(int i=0;i<order2.size();i++)
      order2[i] = i;

    std::sort(order2.begin(),order2.end(),sequence_order<double>(total1));
    std::reverse(order2.begin(),order2.end());

    int total=0;
    for(int i=1;i<order2.size();i++) {
      for(int j=0;j<i;j++)
	total += distances[order2[i]][order2[j]];

      std::cerr<<"fraction = "<<double(i)/(order2.size()-1)<<"     AveD = "<<double(total)/(i*i+i)*2<<std::endl;
    }
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
