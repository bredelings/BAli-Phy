#include <cmath>
#include "arguments.H"



double sumMM(const vector<Matrix>& Matrices,const Matrix& Q,int s2, int i,int j) {
  double sum = log_p;
  for(int s1=0;s1<4;s1++)
    sum = logsum(sum,Matrices[s1](i-1,j-1) + Q(s1,s2));
  return sum;
}

double sum_M(const vector<Matrix>& Matrices,const Matrix& Q,int s2, int i,int j) {
  double sum = log_p;
  for(int s1=0;s1<4;s1++)
    sum = logsum(sum,Matrices[s1](i,j-1) + Q(s1,s2));
  return sum;
}

double sumM_(const vector<Matrix>& Matrices,const Matrix& Q,int s2, int i,int j) {
  double sum = log_p;
  for(int s1=0;s1<4;s1++)
    sum = logsum(sum,Matrices[s1](i-1,j) + Q(s1,s2));
  return sum;
}

double sum__(const vector<Matrix>& Matrices,const Matrix& Q,int s2, int i,int j) {
  double sum = log_p;
  for(int s1=0;s1<4;s1++)
    sum = logsum(sum,Matrices[s1](i,j) + Q(s1,s2));
  return sum;
}

vector<int> sample_path(const vector<Matrix>& Matrices,const IndelModel& IModel,int i,int j) {
  vector<int> path;

  return path;
}

vector<int> sample_path_conditional(const IndelModel& IModel, int length1) {
  vector<Matrix> Matrices;

  int delta = length2;
  int dim1 = length1+1;
  int dim2 = length1+delta+1;
  
  for(int i=0;i<4;i++)
    Matrices.push_back(Matrix(dim1,dim2));

  for(int s=0;s<4;s++)
    Matricies[s](0,0) = pi[s];

  for(int i=1;i<dim1;i++) {
    Matrices[0](i,0) = log_0;
    Matrices[1](i,0) = log_0;
    Matrices[2](i,0) = sumM_(Matrices,Q,2,i,0)
    Matrices[3](i,0) = sum__(Matrices,Q,3,i,0)
  }

  for(int i=0;i<dim2;i++) {
    Matrices[0](0,i) = log_0;
    Matrices[1](0,i) = sum_M(Matrices,Q,1,i,0)
    Matrices[2](0,i) = log_0;
    Matrices[3](0,i) = sum__(Matrices,Q,3,i,0)
  }

  for(int n=0;n<std::max(dim1,dim2);n++) {
    if (n<dim2) 
      for(int i=1;i<n and i<dim1;i++) {
	Matrices[0](i,n) = sumMM(Matrices,Q,0,i,n);
	Matrices[1](i,n) = sum_M(Matrices,Q,1,i,n);
	Matrices[2](i,n) = sumM_(Matrices,Q,2,i,n);
	Matrices[3](i,n) = sum__(Matrices,Q,3,i,n);
      }

    if (n<dim1) 
      for(int i=1;i<=n and i<dim2;i++) {
	Matrices[0](i,n) = sumMM(Matrices,Q,0,n,i);
	Matrices[1](i,n) = sum_M(Matrices,Q,1,n,i);
	Matrices[2](i,n) = sumM_(Matrices,Q,2,n,i);
	Matrices[3](i,n) = sum__(Matrices,Q,3,n,i);
      }
  }

  //choose length2

  //back-sample
  vector<int> path = sample_path(Matrices,IModel,length1,length2);

  return path;
}


int main() {

  // Initial length = ??     Probably from args;

  // Do this in backwards order, so that we know what length we have.
  for(int i=0;i<T.branches();i++) {
    
  }
  
}
