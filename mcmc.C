#include "mcmc.H"
#include "sample.H"
#include "likelihood.H"

using std::valarray;
static int total_samples = 0;


// make a more condensed output format!
// This method of printing alignments is too large
void print_alignments(const alignment& A,const string& s1,const string& s2) {
  int n1 = A.index(s1);
  assert(n1 != -1);

  int n2 = A.index(s2);
  assert(n2 != -1);

  int pos1=0,new_pos1=0;
  int pos2=0,new_pos2=0;
  for(int column=0;column<A.length();column++) {
    if (A(column,n1) == alphabet::gap && A(column,n2) == alphabet::gap) 
      continue;

    if (A(column,n1) != alphabet::gap) 
      new_pos1++;
    if (A(column,n2) != alphabet::gap) 
      new_pos2++;

    std::cout<<s1<<" "<<s2<<" :     "<<pos1<<" "<<pos2<<"   "<<new_pos1<<"   "<<new_pos2<<endl;
	     
    pos1 = new_pos1;
    pos2 = new_pos2;
  }
}

/* 
  print_alignments(A,"CAR4081","consGenv");
  print_alignments(A,"CAR4081","consAenv");
  print_alignments(A,"CAR4081","consBenv");
  print_alignments(A,"consGenv","consAenv");
  print_alignments(A,"consGenv","consBenv");
  print_alignments(A,"consAenv","consBenv");
*/

void print_alignments(const alignment& A,const Parameters& Theta) {
  total_samples++;

  print_alignments(A,"H_sapiens","Sulfaci1");
  print_alignments(A,"H_sapiens","Halomari");
  print_alignments(A,"H_sapiens","Esch_coli3");
  print_alignments(A,"Sulfaci1","Halomari");
  print_alignments(A,"Sulfaci1","Esch_coli3");
  print_alignments(A,"Halomari","Esch_coli3");
  
}

inline int aid(const alignment& A) {
  int id=0;
  for(int species=0;species < A.size2();species++) {
    for(int column=0;column < A.length();column++) {
      id = A(column,species)+ id*684822857;
    }
  }
  return id;
}

void print_stats(const alignment& A,const Parameters& Theta,
		 double probability(const alignment&,const Parameters&)) {
  std::cerr<<"previous = "<<
    probability_no_tree(A,Theta)<<"  "<<
    probability_simple_tree(A,Theta)<<"  "<<
    probability3(A,Theta)<<"  "<<
    probability(A,Theta)<<endl
	   <<"old  ["<<probability2(A,Theta)<<": "<<prior_internal(A,Theta)<<" + "<<substitution(A,Theta)<<"]"<<endl
	   <<"HMM  ["<<probability3(A,Theta)<<": "<<prior_HMM(A,Theta)<<" + "<<substitution(A,Theta)<<"]"<<endl;
  
  std::cerr<<A<<endl;
}

valarray<double> autocorrelation(valarray<double> v) {
  double mean = v.sum()/v.size();
  v -= mean;
  valarray<double> w(v.size()/2);
  for(int i=0;i<w.size();i++) {
    double sum1=0;
    double sum2=0;
    double sum3=0;
    for(int j=0;j<v.size()-i;j++) {
      sum1 += v[j]*v[j+i];
      sum2 += v[j]*v[j];
      sum3 += v[j+i]*v[j+i];
    }
    w[i] = sum1/sqrt(sum2*sum3);
  }
  return w;
}


void MCMC(alignment& A,Parameters& Theta,
	   const int max,double probability(const alignment&,const Parameters&)) {
  const SequenceTree& T = Theta.T;
  SequenceTree ML_tree = T;
  alignment ML_alignment = A;
  bool ML_printed = true;
  
  A.create_internal(T);
  std::cerr<<A<<endl;

  SequenceTree Sum = T;

  const int correlation_time = int(T.leaves()*log(T.leaves()));
  const int start_after = int( 600.0*T.leaves()*log(T.leaves()) );
  int total_samples = 0;

  double p=probability(A,Theta);
  double ML_score = p;
  double new_p=0;

  print_stats(A,Theta,probability);

  valarray<double> v(p,5000);

  for(int iterations=0; iterations < max; iterations++) {
    int id = aid(A);
    std::cerr<<"iterations: "<<iterations<<"    logp = "<<p<<"      id = "<<id<<endl;

    v = v.shift(-1);
    v[0] = p;

    /******************** Record Statistics *******************/
    if (iterations > start_after) {


      if (iterations%correlation_time == 0) 
      	print_alignments(A,Theta);

      for(int i=0;i<T.num_nodes();i++)
	Sum.branch(i).length += T.branch(i).length;
      total_samples++;

      if (iterations % 100 == 0) {
	SequenceTree Average = Sum;
	for(int i=0;i<T.num_nodes();i++)

	std::cout<<"------begin tree---------\n";
	std::cout<<Average<<endl;
	std::cout<<"------end tree---------\n";
      }
    }


    /******************* Propose new position *********************/
    alignment A2 = A;
    Parameters Theta2 = Theta;

    sample(A2,Theta2);

    new_p = probability(A2,Theta2);

    if (new_p > ML_score) {
      // arguably I could optimize these for a few iterations
      ML_score = new_p;
      ML_tree = T;
      ML_alignment = A2;

      ML_printed = false;
    }

    if (!ML_printed and iterations % 100 == 0) {
      std::cerr<<"ML = "<<ML_score<<endl;
      std::cerr<<ML_alignment<<endl;
      std::cerr<<ML_tree<<endl;
      ML_printed = true;
    }

    /***************** Print Diagnostic Output ********************/
    if (iterations %50 == 0 or std::abs(p - new_p)>8) {
      valarray<double> w = autocorrelation(v);
      for(int i=0;i<w.size();i++) {
	std::cout<<i<<"   "<<w[i]<<std::endl;
      }

      print_stats(A,Theta,probability);
      print_stats(A2,Theta2,probability);

      A2.print_fasta(std::cerr);
    }


    /*****************Actually Move to new position ***************/
    A = A2;
    Theta = Theta2;
    p = new_p;
  }
  std::cerr<<"total samples = "<<total_samples<<endl;
}


