#include "rng.H"
#include "likelihood.H"
#include "sample.H"
#include "mcmc.H"
#include "util.H"

//FIXME - are we not guaranteed that leaf nodes will be the children of
// their branch?

MCMC::result_t do_MH_move(const alignment& A,Parameters& P,const Parameters& P2) {
  
  double p1 = probability3(A,P);
  double p2 = probability3(A,P2);
  
  std::cerr<<" MH ["<<p2-p1<<"] : ";

  if (myrandomf() < exp(p2-p1)) {
    P=P2;
    std::cerr<<"accepted\n";
    return MCMC::success;
  }
  else {
    std::cerr<<"rejected\n";
    return MCMC::failure;
  }
}

MCMC::result_t slide_branch_length(const alignment& A, Parameters& P,int b,bool up) {
  const SequenceTree& T = P.T;

  /*--------------- Find the branch names ----------------*/
  int b2 = -1;
  int b3 = -1;

  if (up) {
    int child = T.branch(b).child();
    int parent = T.branch(b).parent();
    if ((int)T.branch_up(child) == (int)T.branch_up(parent)) {
      if (not T[parent].has_left())
	return MCMC::failure;
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
    if (b < T.leaves()) return MCMC::failure;
    b2 = T.branch(b).child().left();
    b2 = T.branch_up(b2);

    b3 = P.T.branch(b).child().right();
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

  /*------------------Calculate P2-------------------*/
  Parameters P2 = P;
  P2.setlength(b,newlength);
  P2.setlength(b2,T.branch(b2).length()-epsilon);
  P2.setlength(b3,T.branch(b3).length()-epsilon);
  
  /*--------------- Do the M-H step if OK---------------*/
  
  MCMC::result_t r = do_MH_move(A,P,P2);
  if (r == MCMC::success) 
    std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<endl;
  else
    std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<endl;
  return r;
}


MCMC::result_t change_branch_length(const alignment& A, Parameters& P,int b) {

    Parameters P2 = P;
    /********* Propose increment 'epsilon' ***********/
    const double sigma = 0.3/2;
    const double length = P2.T.branch(b).length();
    double newlength = length + gaussian(0,sigma);
    if (newlength<0) newlength = -newlength;

    /******** Calculate propsal ratio ***************/
    P2.setlength(b,newlength);

    /********** Do the M-H step if OK**************/
    MCMC::result_t r = do_MH_move(A,P,P2);
    if (r == MCMC::success) 
      std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<endl;
    else
      std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<endl;
    return r;
}

MCMC::result_t change_branch_length_and_T(alignment& A, Parameters& P,int b) {

    /********* Propose increment 'epsilon' ***********/
    const double sigma = 0.4/2;
    const double length = P.T.branch(b).length();
    double newlength = length + gaussian(0,sigma);

    std::cerr<<" old length = "<<P.T.branch(b).length()<<"  new length = "<<newlength<<std::endl;\

    // If the length is positive, simply propose a length change
    if (newlength >= 0) {
      Parameters P2 = P;
      P2.setlength(b,newlength);

      /********** Do the M-H step if OK**************/
      MCMC::result_t r = do_MH_move(A,P,P2);
      if (r == MCMC::success) 
	std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<endl;
      else
	std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<endl;
      return r;
    }
    // If the length is NOT positive, then propose a T change as well
    else {
      vector<int> nodes = get_nodes(A,P.T,b);

      /****** Generate the Different Topologies *******/
      SequenceTree T2 = P.T;
      SequenceTree T3 = P.T;
      
      T2.exchange(nodes[1],nodes[2]);
      T3.exchange(nodes[1],nodes[3]);

      T2.branch(b).length() = -newlength;
      T3.branch(b).length() = -newlength;

      return sample_topology(A,P,T2,T3,b);
    }
}

