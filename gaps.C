#include <cmath>
#include "likelihood.H"
#include "possibilities.H"

const double gap_init = exp(-16);
const double gap_extend = exp(-4);

double sum(const possibility* edges,const vector<possibility>& prev,
	   const vector<double>& accum) {
  double sum=0;

  if (edges) {
    if (!prev.size())
      sum = pow(gap_init,edges->size());
    else {
      for(int i=0;i<prev.size();i++) {
	const possibility& prev_edges = prev[i];
	
	int extended = num_shared(prev_edges,*edges);
	int init = edges->size() - extended;
	sum += accum[i+1] * pow(gap_extend,extended) * pow(gap_init,init);
      }      
    }
  }
  else {
    if (!prev.size())
      sum = 1.0;
    else {
      for(int i=0;i<prev.size();i++)
	sum += accum[i+1];
    }
  }

  return sum;
}


void do_sum(vector<double>& accum,const vector<double>& accum2,
	    const vector<possibility>& possibilities,const vector<possibility>& possibilities2) {

  // for each configuration, sum over possibilites for previous column
  double total = 0.0;
  for(int i=1;i<accum.size();i++) {
    const possibility* edges = 0;
    if (possibilities.size())
      edges = &possibilities[i-1];
    accum[i] = sum(edges,possibilities2,accum2);
    total += accum[i];
  }

  for(int i=1;i<accum.size();i++) {
    accum[i] /= total;
  }
  accum[0] = accum2[0] + log(total);
}

vector<int> get_boundaries(const alignment& A,int first,int last) {

  const int sign = (first<=last)?1:-1;
  const int end = last + sign;

  vector<int> boundary(A.num_sequences(),end);

  bool done=false;
  for(int column=first;column != end && !done;column += sign) {
    done = true;
    for(int i=0;i<A.num_sequences();i++) {
      if (boundary[i] != end) continue;
      done = false;

      if (A(column,i) == alphabet::gap) continue;
      for(int j=0;j<A.num_sequences();j++) {
	if (i==j) continue;
	if (A(column,j) == alphabet::gap && boundary[j] == end) continue;

	boundary[i] = column;
	break;
      }
    }
  }

  return boundary;
}


// Version 1.0
//  1. Ignore branch lengths - just use penalty!
double prior(const alignment& A,const Parameters& Theta) {
  const tree& T = Theta.T;

  assert(A.num_sequences() == T.leaves());
  vector<int> start = get_boundaries(A,0,A.length()-1);
  vector<int> end   = get_boundaries(A,A.length()-1,0);


  //Step 1: construct the list of possible indels for each column
  vector< vector<possibility> > possibilities(A.length()+1);
  for(int column=0;column<A.length();column++) {
    vector<int> present(A.num_sequences());
    for(int i=0;i<A.num_sequences();i++) {
      present[i] = (A(column,i) != alphabet::gap);

      if (!present[i] && !(start[i] <= column && column <= end[i]) )
	present[i] = -1;
    }
    possibilities[column] = get_possibilities(present,T);
  }

  // Step 2: Move across the alignment, updating a running max or sum
  //  * Since each row depends only on the previous row, we can keep the max
  //    up to the current point only in terms of the choice for the previous
  //    row as an unbound variable
  //  * Actually, there might be an insertion INSIDE this current insertion
  //    so we need to deal w/ that also.


  vector< vector<double> > accumulation(A.length()+1);
  for(int i=0;i<accumulation.size();i++) {
    int size = possibilities[i].size();
    if (!size) size=1;
    accumulation[i].resize(size+1,0);
  }

  {  // First column gets special handling
    vector<double> start(2);start[0]=0.0;start[1]=1.0;
    do_sum(accumulation[0],start,
	   possibilities[0],vector<possibility>());
  }

  for(int column=1;column < accumulation.size();column++) 
    do_sum(accumulation[column],accumulation[column-1],
	   possibilities[column],possibilities[column-1]);
  
  return accumulation[A.length()][0];
}
