#include "sample.H"
#include "rng.H"
#include <algorithm>
#include "mcmc.H"
#include "3way.H"
#include "likelihood.H"
#include "util-random.H"

MCMC::result_t change_branch_length_move(alignment& A, Parameters& P,int b) {
  if (not P.SModel().full_tree and b>=P.T.n_leaves())
    return MCMC::result_t(0.0,6); // no_result

  return change_branch_length(A,P,b);
}

MCMC::result_t change_branch_length_multi_move(alignment& A, Parameters& P,int b) {
  if (not P.SModel().full_tree and b>=P.T.n_leaves())
    return MCMC::result_t(0.0,6); // no_result

  return change_branch_length_multi(A,P,b);
}

MCMC::result_t sample_tri_one(alignment& A, Parameters& P,int b) {
  assert(P.IModel().full_tree); 

  const SequenceTree& T = P.T;

  int node1 = T.branch(b).target();
  int node2 = T.branch(b).source();

  if (myrandomf() < 0.5)
    std::swap(node1,node2);

  if (node1 < T.n_leaves())
    std::swap(node1,node2);
    
  A = tri_sample_alignment(A,P,node1,node2);

  return MCMC::result_t(); // no_result
}

MCMC::result_t sample_tri_branch_one(alignment& A, Parameters& P,int b) {
  if (not P.SModel().full_tree and b>=P.T.n_leaves())
    return MCMC::result_t(0.0,4); // no_result

  MCMC::result_t result(0.0,4);
  result[0] = 1.0;
  result[2] = 1.0;

  assert(P.IModel().full_tree); 

  const SequenceTree& T = P.T;

  int node1 = T.branch(b).target();
  int node2 = T.branch(b).source();

  if (myrandomf() < 0.5)
    std::swap(node1,node2);

  if (node1 < T.n_leaves())
    std::swap(node1,node2);
    
  const double sigma = 0.3/2;
  double length1 = T.branch(b).length();
  double length2 = length1 + gaussian(0,sigma);
  if (length2 < 0) length2 = -length2;

  if (tri_sample_alignment_branch(A,P,node1,node2,b,length2)) {
    result[1] = 1;
    result[3] = std::abs(length2 - length1);
  }

  return result;
}


MCMC::result_t sample_alignments_one(alignment& A, Parameters& P,int b) {
  assert(P.IModel().full_tree); 

  A = sample_alignment(A,P,b);
  return MCMC::result_t(); // no_result
}

MCMC::result_t sample_node_move(alignment& A, Parameters& P,int node) {
  assert(P.IModel().full_tree); 

  A = sample_node(A,P,node);

  return MCMC::result_t(); // no_result
}

MCMC::result_t sample_two_nodes_move(alignment& A, Parameters& P,int n0) {
  assert(P.IModel().full_tree); 

  vector<int> nodes = A3::get_nodes_random(P.T,n0);
  int n1 = -1;
  for(int i=1;i<nodes.size();i++)
    if (P.T[ nodes[i] ].is_internal_node()) {
      n1 = nodes[i];
      break;
    }
  assert(n1 != 1);

  int b = P.T.branch(n0,n1);

  A = sample_two_nodes(A,P,b);

  return MCMC::result_t(); // no_result
}

vector<int> get_cost(const Tree& T) {
  vector<int> cost(T.n_branches()*2,-1);
  vector<const_branchview> stack1; stack1.reserve(T.n_branches()*2);
  vector<const_branchview> stack2; stack2.reserve(T.n_branches()*2);
  for(int i=0;i<T.n_leaves();i++) {
    const_branchview b = T.directed_branch(i).reverse();
    cost[b] = 0;
    stack1.push_back(b);
  }
    
  while(not stack1.empty()) {
    stack2.clear();
    for(int i=0;i<stack1.size();i++)
      append(stack1[i].branches_before(),stack2);

    stack1.clear();

    for(int i=0;i<stack2.size();i++) {
      vector<const_branchview> children;
      append(stack2[i].branches_after(),children);

      assert(children.size() == 2);
      if (cost[children[0]] != -1 and cost[children[1]] != -1) {
	int cost_l = cost[children[0]];
	if (not children[0].is_leaf_branch())
	  cost_l++;

	int cost_r = cost[children[1]];
	if (not children[1].is_leaf_branch())
	  cost_r++;

	if (cost_l > cost_r)
	  std::swap(cost_l,cost_r);

	cost[stack2[i]] = 2*cost_l + cost_r;
	stack1.push_back(stack2[i]);
      }
    }
  }
  
  for(int i=0;i<cost.size();i++)
    assert(cost[i] != -1);

  return cost;
}

vector<int> walk_tree_path(const Tree& T) {

  vector<int> cost = get_cost(T);

  vector<const_branchview> b_stack;
  b_stack.reserve(T.n_branches());
  vector<const_branchview> branches;
  branches.reserve(T.n_branches());
  vector<const_branchview> children;
  children.reserve(3);

  // put a random leaf branch on both the stack and the list
  // FIXME - It would still be good to pick a leaf node on a long diagonal path
  // put a random leaf branch on both the stack and the list
  int leaf = 0;
  leaf = myrandom(T.n_leaves());
  for(int b=0;b<T.n_leaves();b++)
    if (cost[T.directed_branch(b)] < cost[T.directed_branch(leaf)])
      leaf = b;

  assert(T.directed_branch(leaf).source() == leaf);
  b_stack.push_back(T.directed_branch(leaf));

  while(not b_stack.empty()) {
    // pop stack into list
    branches.push_back(b_stack.back());
    b_stack.pop_back();

    // get children of the result
    children.clear();
    append(branches.back().branches_after(),children);
    children = randomize(children);

    // sort children in decrease order of cost
    if (children.size() < 2)
      ;
    else {
      if (children.size() == 2) {
	if (cost[children[0]] < cost[children[1]])
	  std::swap(children[0],children[1]);
      }
      else
	std::abort();
    }
      
    // put children onto the stack
    b_stack.insert(b_stack.end(),children.begin(),children.end());
  }

  assert(branches.size() == T.n_branches());

  vector<int> branches2(branches.size());
  for(int i=0;i<branches.size();i++)
    branches2[i] = branches[i].undirected_name();

  return branches2;
}

MCMC::result_t sample_NNI_and_branch_lengths(alignment& A,Parameters& P) {
  vector<int> branches = walk_tree_path(P.T);

  MCMC::result_t result;

  for(int i=0;i<branches.size();i++) {
    int b = branches[i];

    std::cerr<<"\n\n Processing branch "<<b<<" with root "<<P.LC.root<<endl;

    if (P.T.branch(b).is_internal_branch())
      three_way_topology_sample(A,P,b);
    change_branch_length(A,P,b);
    change_branch_length(A,P,b);
    change_branch_length(A,P,b);
  }

  return result;
}


MCMC::result_t change_parameters(alignment& A,Parameters& P) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  Parameters P2 = P;

  P2.fiddle();

#ifndef NDEBUG  
  for(int i=0;i<P.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P.SModel().parameters()[i];
  std::cerr<<endl;
  std::cerr<<P.probability(A,P);
  std::cerr<<endl<<endl;

  for(int i=0;i<P2.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P2.SModel().parameters()[i];
  std::cerr<<endl;
  std::cerr<<P.probability(A,P2);
  std::cerr<<endl<<endl;
#endif

  if (P.accept_MH(A,P,A,P2)) {
    P = P2;
    result[1] = 1;
#ifndef NDEBUG
    std::cerr<<"success\n";
#endif
  }
  else {
#ifndef NDEBUG
    std::cerr<<"failure\n";
#endif
  }
  return result;
}

MCMC::result_t change_gap_parameters(alignment& A,Parameters& P) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  Parameters P2 = P;
  P2.IModel().fiddle(P.i_fixed);

  if (P.accept_MH(A,P,A,P2)) {
    P = P2;
    result[1] = 1;
  }

  return result;
}




MCMC::result_t sample_frequencies(alignment& A,Parameters& P) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  Parameters P2 = P;
  valarray<double> f = P2.SModel().frequencies();
  f = dirichlet_fiddle(f,0.25/sqrt(f.size()));
  P2.SModel().frequencies(f);
  P2.recalc();

  if (P.accept_MH(A,P,A,P2)) {
    P = P2;
    result[1] = 1;
  }

  return result;
}
