#include <cassert>
#include <cmath>
#include <vector>
#include <iostream>
#include "log-double.H"
#include <valarray>
#include "le-double.H"
#include "mytypes.H"

using namespace std;

template <class T>
class dpmatrix 
{
  int s1;
  int s2;
  int s3;

  valarray<T> data;
public:
  int size1() const {return s1;}
  int size2() const {return s2;}
  int size3() const {return s3;}

  T& operator()(int i,int j,int k) {return data[i*s2*s3+j*s3+k];}
  const T& operator()(int i,int j,int k) const {return data[i*s2*s3+j*s3+k];}

  dpmatrix(int i1,int i2,int i3):s1(i1),s2(i2),s3(i3),data(i1*i2*i3) {}
};



template<typename T> 
T propagate(dpmatrix<T>& F,const ublas::matrix<T>& G) 
{
  int I = F.size1()-1;
  int J = F.size2()-1;
  for(int t=0;t<50;t++) {

  const T zero = 0;

  for (int i = 0; i <= I;i++) 
    for(int S=0;S<3;S++)
      F(i,0,S) = zero;

  for (int j = 0; j <= J;j++) 
    for(int S=0;S<3;S++)
      F(0,j,S) = zero;

  F(0,0,0) = 1;
  F(0,0,1) = 0;
  F(0,0,2) = 0;


  for (int i = 1; i <= I;i++) 
    for (int j = 1; j <= J;j++)
      for(int S=0;S<3;S++) {
	int i1=i;
	int j1=j;
	if (S==0 or S==1)
	  i1--;
	if (S==0 or S==2)
	  j1--;

	//	T x = F(i1,j1,0)*G(0,S) + F(i1,j1,1)*G(1,S) + F(i1,j1,2)*G(2,S);
	  
	T x = zero;
	for(int S1=0;S1<3;S1++)
	  x += F(i1,j1,S1)*G(S1,S);
      
	F(i,j,S) = x;
      }

  }
  return F(I,J,0) + F(I,J,1) + F(I,J,2);

}


int main(int argn, char *argv[])
{
  le_double_t::initialize();

  ublas::matrix<double> G(3,3);

  G(0,0) = 0.99;
  G(0,1) = 0.005;
  G(0,2) = 0.005;

  G(1,0) = 0.8;
  G(1,1) = 0.2;
  G(1,2) = 0;

  G(2,0) = 0.8;
  G(2,1) = 0;
  G(2,2) = 0.2;

  ublas::matrix<long double> G_long = G;
  ublas::matrix<le_double_t> G_le = G;
  ublas::matrix<log_double_t> G_log = G;

  const int L = 400;

  dpmatrix<double> F(L,L,3);
  dpmatrix<long double> F_long(L,L,3);
  dpmatrix<le_double_t> F_le(L,L,3);
  dpmatrix<log_double_t> F_log(L,L,3);

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

