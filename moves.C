#include "likelihood.H"
#include "sample.H"
#include "rng.H"
#include <algorithm>

void change_branch_length(const alignment& A, Parameters& Theta,int b) {

    Parameters Theta2 = Theta;
    /********* Propose increment 'epsilon' ***********/
    const double sigma = 0.3/2;
    const double length = Theta2.T.branch(b).length();
    double lower = length - sigma;
    double higher = length + sigma;
    if (lower < 0) lower = 0;
    double newlength = lower + myrandomf()*(higher-lower);

    /******** Calculate propsal ratio ***************/
    Theta2.setlength(b,newlength);

    double new_lower = newlength - sigma;
    double new_higher = newlength + sigma;
    if (new_lower < 0) new_lower = 0;
    double density12 = 1.0/(higher - lower);
    double density21 = 1.0/(new_higher - new_lower);

    /********** Do the M-H step if OK**************/
    double lL_1 = substitution(A,Theta) + prior(Theta);
    double lL_2 = substitution(A,Theta2) + prior(Theta);

    // accept w/ probability (a2/a1)*(p21/p12)
    if (myrandomf() < exp(lL_2 - lL_1)*density21/density12) {
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

void move_node(const alignment& A, Parameters& Theta,int node) {
  const tree& T = Theta.T;
  int b1 = node;
  int b2 = T[node].left();
  int b3 = T[node].right();
  
  double r = myrandomf()*3;
  if (r < 1) {
    int temp = b1;
    b1 = b2;
    b2 = b3;
    b3 = temp;
  }
  else if (r<2) {
    int temp = b1;
    b1 = b3;
    b3 = b2;
    b2 = temp;
  }

  // change the length of branch b1, using other two to pick up slack
  double min = -T.branch(b1).length();
  double max = std::max(T.branch(b2).length(),T.branch(b3).length());
  double s = 1.0/4;
  if (min < -s) min = -s;
  if (max > s) max = s;

  double delta = min * myrandomf()*(max-min);

  Parameters Theta2 = Theta;

  Theta2.setlength(b1,T.branch(b1).length() + delta);
  Theta2.setlength(b2,T.branch(b2).length() - delta);
  Theta2.setlength(b3,T.branch(b3).length() - delta);
}

void sample(alignment& A,Parameters& Theta) {

  double r = myrandomf();
  if (r < 0.25) {
    int b = myrandom(Theta.T.branches());

    A = sample_alignment(A,Theta,b);
  }
  else if (r < 0.5) {
    int node = myrandom(Theta.T.leaves(),Theta.T.num_nodes()-1);
    A = sample_node(A,Theta,node);
  }
  else if (r<0.75) {
    int b = myrandom(Theta.T.leaves(),Theta.T.branches());
    sample_topology(A,Theta,b);
  }
  else {
    int b = myrandom(Theta.T.branches());
    if (b<Theta.T.leaves())
      change_branch_length(A,Theta,b);
    else
      change_branch_length_and_T(A,Theta,b);
  }
}

