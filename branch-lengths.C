#include "rng.H"
#include "likelihood.H"
#include "sample.H"

void change_branch_length(const alignment& A, Parameters& Theta,int b) {

    Parameters Theta2 = Theta;
    /********* Propose increment 'epsilon' ***********/
    const double sigma = 0.3/2;
    const double length = Theta2.T.branch(b).length();
    double newlength = length + gaussian(0,sigma);
    if (newlength<0) newlength = -newlength;

    /******** Calculate propsal ratio ***************/
    Theta2.setlength(b,newlength);

    /********** Do the M-H step if OK**************/
    double lL_1 = substitution(A,Theta) + prior(Theta);
    double lL_2 = substitution(A,Theta2) + prior(Theta);

    // accept w/ probability (a2/a1)*(p21/p12)
    if (myrandomf() < exp(lL_2 - lL_1)) {
      Theta=Theta2;
      std::cerr<<Theta2.T.branch(b).length()<<" "<<Theta.T.branch(b).length()<<endl;
      std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
      
    }
    else
      std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
}

void change_branch_length_and_T(alignment& A, Parameters& Theta,int b) {

    /********* Propose increment 'epsilon' ***********/
    const double sigma = 0.4/2;
    const double length = Theta.T.branch(b).length();
    double newlength = length + gaussian(0,sigma);

    std::cerr<<" old length = "<<Theta.T.branch(b).length()<<"  new length = "<<newlength<<std::endl;\

    // If the length is positive, simply propose a length change
    if (newlength >= 0) {
      Parameters Theta2 = Theta;
      Theta2.setlength(b,newlength);

      /********** Do the M-H step if OK**************/
      double lL_1 = substitution(A,Theta) + prior(Theta);
      double lL_2 = substitution(A,Theta2) + prior(Theta);
      
      // accept w/ probability (a12/a21)*(p21/p12)
      if (myrandomf() < exp(lL_2 - lL_1)) {
	Theta=Theta2;
	std::cerr<<Theta2.T.branch(b).length()<<" "<<Theta.T.branch(b).length()<<endl;
	std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
	
      }
      else
	std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
    }
    // If the length is NOT positive, then propose a T change as well
    else {
      vector<int> nodes = get_nodes(A,Theta.T,b);

      /****** Generate the Different Topologies *******/
      SequenceTree T2 = Theta.T;
      SequenceTree T3 = Theta.T;
      
      T2.exchange(nodes[1],nodes[2]);
      T3.exchange(nodes[1],nodes[3]);

      T2.branch(b).length() = -newlength;
      T3.branch(b).length() = -newlength;

      sample_topology(A,Theta,T2,T3,b);
    }
}

