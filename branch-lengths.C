#include "rng.H"
#include "sample.H"
#include "mcmc.H"
#include "util.H"
#include "5way.H"


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

    
    /****** Generate the Different Topologies *******/
    Parameters P2 = P;
    
    SequenceTree& T2 = P2.T;
    
    vector<int> nodes = A5::get_nodes_random(P.T,b);
    T2.exchange(nodes[1],nodes[2]);
    
    P2.setlength(b,-newlength);
    
    if (two_way_topology_sample(A,P,P2,b)) {
      result[1] = 1;
      result[5] = 1;
      result[9] = std::abs(length - newlength);
    }
  }
  return result;
}

int find_branch(const tree& T,int node1, int node2) {
  for(int b=0;b<T.branches();b++) {
    if (T.branch(b).child() == node1 and T.branch(b).parent() == node2)
      return b;
    if (T.branch(b).child() == node2 and T.branch(b).parent() == node1)
      return b;
  }
  throw myexception()<<"Couldn't find branch with nodes "<<node1<<" and "<<node2;
}


/// Walk a subtree across the tree - (different kind of sliding that other 'slide' routine)
MCMC::result_t slide_branch_and_T(alignment& A, Parameters& P,int branch) {
  MCMC::result_t result(0.0,10);

  result[0] = 1.0;
  
  SequenceTree& T1 = P.T;

  /*------- Propose increment 'epsilon' ----------*/
  const double sigma = P.branch_mean/2;
  double epsilon = gaussian(0,sigma);


  /*------- Pick a direction ---------------*/

  int node = T1.branch(branch).child();
  if (T1[node].leaf() or myrandomf()< 0.5)
    node = T1[node].parent();
  assert(not T1[node].leaf());

  /*------- Find adjacent branches ---------------*/

  vector<int> branches;
  for(int b=0;b<T1.branches();b++) {
    if (T1.branch(b).child() == node or T1.branch(b).parent() == node) {
      if (b == branch) continue;
      branches.push_back(b);
    }
  }
  assert(branches.size() == 2);
  int b1 = branches[0];
  int b2 = branches[1];
 

  /*-------------- Make new tree -----------------*/

  double newlength1 = T1.branch(b1).length() + epsilon;
  double newlength2 = T1.branch(b1).length() - epsilon;

  // Just change the branch length - this is the simple case
  if (newlength1 >= 0 and newlength2 >= 0) {
    Parameters P2 = P;
    SequenceTree& T2 = P2.T;
    P2.setlength(b1,newlength1);
    P2.setlength(b2,newlength2);
    if (do_MH_move(A,P,P2)) {
      std::cerr<<" slide :  ("<<T1.branch(b1).length()<<","<<T1.branch(b2).length()<<")  ->  ";
      std::cerr<<        "  ("<<T2.branch(b1).length()<<","<<T2.branch(b2).length()<<")\n";
      
      result[1] = 1;
      result[3] = 1;
      result[7] = std::abs(epsilon);
    }
    else {
      std::cerr<<" slide :  ("<<T1.branch(b1).length()<<","<<T1.branch(b2).length()<<")  -/->  ";
      std::cerr<<        "  ("<<T2.branch(b1).length()<<","<<T2.branch(b2).length()<<")\n";
    }

  }
  // Move branch 'branch' across branch b1
  else if (newlength1 < 0) {
    Parameters P2 = P;
    SequenceTree& T2 = P2.T;

    vector<int> nodes = A5::get_nodes_random(T1,b1);
    if (nodes[2]==node or nodes[3]==node) {
      std::swap(nodes[0],nodes[2]);
      std::swap(nodes[1],nodes[3]);
      std::swap(nodes[4],nodes[5]);
    }
    if (nodes[1] == node) {
      std::swap(nodes[0],nodes[1]);
    }
    assert(nodes[0] == node);
    int b3 = find_branch(T1,nodes[5],nodes[3]);
    double alpha = wrap(-newlength1,T1.branch(b3).length());


    T2.exchange(nodes[0],nodes[2]);

    // middle branch is now length alpha
    P2.setlength(b1,alpha);
    // sister branch adds in former middle length to keep sum of lengths constant.
    P2.setlength(b2,T1.branch(b2).length()+T1.branch(b1).length());
    // the branch we moved to has lost alpha from its length
    P2.setlength(b3,T1.branch(b3).length()-alpha);

    if (two_way_topology_sample(A,P,P2,branch)) {
      result[1] = 1;
      result[5] = 1;
      result[9] = std::abs(epsilon);
    }
  }
  else if (newlength2 < 0) {
  }
  else {
    std::abort();
  }

  return result;
}

