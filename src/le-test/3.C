#include <cassert>
#include <cmath>
#include <vector>
#include <iostream>
#include "log-double.H"
#include "le-double.H"
#include "mytypes.H"

using namespace std;

template<typename T> 
T propagate(ublas::matrix<T>& F,const ublas::matrix<T>& G) {

  for(int t = 0;t<5000;t++) {
  for (int i = 1; i < F.size1();i++) 
    for (int j = 0; j < F.size2();j++) {
      T x = 0;
      for (int k = 0; k < F.size2();k++)
	x += F(i-1,k)*G(k,j);
      F(i,j) = x;
    }
  }
  return F(F.size1()-1,0);
}


int main(int argn, char *argv[])
{
  le_double_t::initialize();

  ublas::matrix<double> G(3,3);

  for(int i=0;i<G.size1();i++)
    for(int j=0;j<G.size2();j++)
      if (i==j)
	G(i,j) = 0.9/2;
      else
	G(i,j) = 0.05/2;

  ublas::matrix<long double> G_long = G;
  ublas::matrix<log_double_t> G_log = G;
  ublas::matrix<le_double_t> G_le = G;

  ublas::matrix<double> F(1000,3);
  F(0,0) = 0.99;
  F(0,1) = 0.009;
  F(0,2) = 0.001;

  ublas::matrix<long double> F_long = F;
  ublas::matrix<log_double_t> F_log = F;
  ublas::matrix<le_double_t> F_le = F;

  if (argn == 1)
    {
      printf("Usage: pr.bench 1|2|3 \n");
      exit(-1);
    }

  int s = atoi(argv[1]);

  double result;
  if (s == 1) { /* Simple (double) addition */
    std::cout<<"double addition\n";
    result=propagate(F,G);
  }
  else if (s==2) {
    std::cout<<"long double addition\n";
    result=propagate(F_long,G_long);
  }
  else if (s==3) {
    std::cout<<"Ben's extended-exponent addition\n";
    result=propagate(F_le,G_le);
  }
  else if (s==4) {
    std::cout<<"Ben's logarithmic addition\n";
    result=propagate(F_log,G_log);
  }
  else {
      printf("Please specify a number between 1 and 3.\n");
      exit(-1);
  }

  std::cout<<"result = "<<result<<std::endl;
  return 0;
}

