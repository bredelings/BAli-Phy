#include "rng.H"
#include "sample.H"
#include "mcmc.H"
#include "util.H"

//FIXME - are we not guaranteed that leaf nodes will be the children of
// their branch?

bool do_MH_move(const alignment& A,Parameters& P,const Parameters& P2) {
  if (P.accept_MH(A,P,A,P2)) {
    P=P2;
    std::cerr<<"accepted\n";
    return true;
  }
  else {
    std::cerr<<"rejected\n";
    return false;
  }
}

MCMC::result_t slide_branch_length(const alignment& A, Parameters& P,int b,bool up) {
  const SequenceTree& T = P.T;

  MCMC::result_t result(0.0,6);
  result[0] = 1.0;
  result[2] = 1.0;
  result[4] = 1.0;

  /*--------------- Find the branch names ----------------*/
  int b2 = -1;
  int b3 = -1;

  if (up) {
    int child = T.branch(b).child();
    int parent = T.branch(b).parent();
    if ((int)T.branch_up(child) == (int)T.branch_up(parent)) {
      if (not T[parent].has_left())
	return result; //failure
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
    if (b < T.leaves()) return result; //failure
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
  
  bool success = do_MH_move(A,P,P2);
  if (success) {
    std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<endl;
    result[1] = 1;
    result[3] = std::abs(newlength - length);
    result[5] = std::abs(log((newlength+0.001)/(length+0.001)));
    result[5] += std::abs(log((T.branch(b2).length()+0.001)/(T.branch(b2).length()+epsilon+0.001)));
    result[5] += std::abs(log((T.branch(b3).length()+0.001)/(T.branch(b3).length()+epsilon+0.001)));
  }
  else 
    std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<endl;
  return result;
}


MCMC::result_t change_branch_length(const alignment& A, Parameters& P,int b) {
  MCMC::result_t result(0.0,6);
  result[0] = 1.0;
  result[2] = 1.0;
  result[4] = 1.0;
  
  Parameters P2 = P;
  /********* Propose increment 'epsilon' ***********/
  const double sigma = 0.3/2;
  const double length = P2.T.branch(b).length();
  double newlength = length + gaussian(0,sigma);
  if (newlength<0) newlength = -newlength;
  
  /******** Calculate propsal ratio ***************/
  P2.setlength(b,newlength);
  
  /********** Do the M-H step if OK**************/
  if (do_MH_move(A,P,P2)) {
    std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<endl;
    result[1] = 1;
    result[3] = std::abs(length - newlength);
    result[5] = std::abs(log((newlength+0.001)/(length+0.001)));
  }
  else
    std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<endl;
  return result;
}

MCMC::result_t change_branch_length_and_T(alignment& A, Parameters& P,int b) {
  MCMC::result_t result(0.0,10);

  if (not P.SModel().full_tree)
    return result; //no_result

  result[0] = 1.0;
  

  /********* Propose increment 'epsilon' ***********/
  const double sigma = 0.4/2;
  const double length = P.T.branch(b).length();
  double newlength = length + gaussian(0,sigma);

  std::cerr<<" old length = "<<P.T.branch(b).length()<<"  new length = "<<newlength<<std::endl;

  // If the length is positive, simply propose a length change
  if (newlength >= 0) {
    result[2] = 1.0;
    result[6] = 1.0;

    Parameters P2 = P;
    P2.setlength(b,newlength);

    /********** Do the M-H step if OK**************/

    if (do_MH_move(A,P,P2)) {
      std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<endl;
      result[1] = 1;
      result[3] = 1;
      result[7] = std::abs(newlength - length);
    }
    else
      std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<endl;
  }
  // If the length is NOT positive, then propose a T change as well
  else {
    result[4] = 1.0;
    result[8] = 1.0;

    vector<int> nodes = get_nodes(A,P.T,b);
    
    /****** Generate the Different Topologies *******/
    Parameters P2 = P;
    Parameters P3 = P;
    
    SequenceTree& T2 = P2.T;
    SequenceTree& T3 = P3.T;
    
    T2.exchange(nodes[1],nodes[2]);
    T3.exchange(nodes[1],nodes[3]);
    
    P2.setlength(b,-newlength);
    P3.setlength(b,-newlength);
    
    if (topology_sample2(A,P,P2,P3,b)) {
      result[1] = 1;
      result[5] = 1;
      result[9] = std::abs(length - newlength);
    }
  }
  return result;
}

