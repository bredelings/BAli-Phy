#include "rng.H"
#include "likelihood.H"
#include "sample.H"
#include "mcmc.H"

inline double wrap(double x,double max) {
  // flip around to position x axis
  if (x < 0)
    x = -x;

  // map to [0,2*max)
  int n = (int)(x/(2.0*max));
  x -= n*2.0*max;

  if (x > max)
    x = max*2 - x;

  return x;
}

//FIXME - are we not guaranteed that leaf nodes will be the children of
// their branch?

void slide_branch_length(const alignment& A, Parameters& Theta,int b,bool up) {
  const SequenceTree& T = Theta.T;

  /*--------------- Find the branch names ----------------*/
  int b2 = -1;
  int b3 = -1;
  if (up) {
    int child = T.branch(b).child();
    int parent = T.branch(b).parent();
    if ((int)T.branch_up(child) == (int)T.branch_up(parent)) {
      if (not T[parent].has_left())
	return;
      b2 = T.branch_up(T[parent].left());
      b3 = T.branch_up(T[parent].right());
    }
    else {
      b2 = T.branch_up(T.branch(b).parent());
      b3 = T.branch_up(T.branch(b).parent().right());
      if (b3 == b)
	b3 = T.branch_up(T.branch(b).parent().left());
    }
  }
  else {
    if (b < T.leaves()) return;
    b2 = T.branch(b).child().left();
    b2 = T.branch_up(b2);

    b3 = Theta.T.branch(b).child().right();
    b3 = T.branch_up(b3);
  }

  /*-------------- Find out how much to slide ---------------*/
  const double sigma = 0.3/2;
  const double min = std::min(T.branch(b2).length(),T.branch(b3).length());
  double length = T.branch(b).length();
  const double max = length + min;

  double newlength = length + gaussian(0,sigma);

  newlength = wrap(newlength,max);
  double epsilon = newlength - length;

  /*------------------Calculate Theta2-------------------*/
  Parameters Theta2 = Theta;
  Theta2.setlength(b,newlength);
  Theta2.setlength(b2,T.branch(b2).length()-epsilon);
  Theta2.setlength(b3,T.branch(b3).length()-epsilon);
  
  /*--------------- Do the M-H step if OK---------------*/
  double lL_1 = substitution::Pr(A,Theta) + prior(Theta);
  double lL_2 = substitution::Pr(A,Theta2) + prior(Theta2);
  
  bool success = false;
  if (myrandomf() < exp(lL_2 - lL_1)) {
    Theta=Theta2;
    std::cerr<<Theta2.T.branch(b).length()<<" "<<Theta.T.branch(b).length()<<endl;
    std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
    success = true;
  }
  else
    std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
  
  record_move("length-sample-slide",success);
}


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
    double lL_1 = substitution::Pr(A,Theta) + prior(Theta);
    double lL_2 = substitution::Pr(A,Theta2) + prior(Theta);

    // accept w/ probability (a2/a1)*(p21/p12)
    bool success = false;
    if (myrandomf() < exp(lL_2 - lL_1)) {
      Theta=Theta2;
      std::cerr<<Theta2.T.branch(b).length()<<" "<<Theta.T.branch(b).length()<<endl;
      std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
      success = true;
    }
    else
      std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";

    record_move("length-sample",success);
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
      double lL_1 = substitution::Pr(A,Theta) + prior(Theta);
      double lL_2 = substitution::Pr(A,Theta2) + prior(Theta);
      
      // accept w/ probability (a12/a21)*(p21/p12)
      bool success = false;
      if (myrandomf() < exp(lL_2 - lL_1)) {
	Theta=Theta2;
	std::cerr<<Theta2.T.branch(b).length()<<" "<<Theta.T.branch(b).length()<<endl;
	std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
	success=true;
      }
      else 
	std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<"   ["<<lL_2-lL_1<<"]\n";
      record_move("length-sample-non-negative",success);
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

      bool success = sample_topology(A,Theta,T2,T3,b);
      record_move("t-sample-branch-based",success);
    }
}

