#include "map.H"
#include "sample.H"
#include "likelihood.H"

void MAP(alignment& A,Parameters& Theta,int max,
	double probability(const alignment&,const Parameters&)) {
  A.create_internal(Theta.T);
  std::cerr<<A<<endl;

  if (max == -1) 
    max = (int)( Theta.T.leaves()*log(Theta.T.leaves())*100 );

  double p=probability(A,Theta);

  for(int iterations=0; iterations < max; iterations++) {
    std::cerr<<"MLiterations = "<<iterations<<"    logp = "<<p<<std::endl;

    /******************* Propose new position *********************/
    alignment A2 = A;
    Parameters Theta2 = Theta;

    sample(A2,Theta2);

    double new_p = probability(A2,Theta2);

    if (new_p > p) {
      A = A2;
      Theta = Theta2;
      p = new_p;
    }
  }
}


