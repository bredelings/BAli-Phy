#include <cassert>
#include <cmath>
#include <vector>
#include <iostream>
#include "log-double.H"
#include "le-double.H"

const int OUTER = 50000;
const int INNER = 1000;

using namespace std;

template<typename T> 
T accumulate(const vector<T>& d) {
  T result=0;
  T x = 0;
  for (int i = 0; i < OUTER; ++i) 
    {
      for (unsigned int j = 1; j < INNER; ++j) 
	{
	  x += d[j]*d[j-1] + d[j-1];
	}
    }
  result = x;
  return result;
}


int main(int argn, char *argv[])
{
  int	 i, s;

  le_double_t::initialize();

  vector<double> d(INNER);
  vector<long double> ld(INNER);
  vector<log_double_t> bd(INNER);
  vector<le_double_t> cd(INNER);
 
  if (argn == 1)
    {
      printf("Usage: pr.bench 1|2|3 \n");
      exit(-1);
    }

  for (i = 0; i < INNER; ++i) 
    {
      d[i] = double(1+i) / INNER;
      ld[i] = d[i];
      bd[i] = d[i];
      cd[i] = d[i];
    }

  s = atoi(argv[1]);

  double result=-1;
  if (s == 1) { /* Simple (double) addition */
    std::cout<<"double addition\n";
    result=accumulate<double>(d);
  }
  else if (s == 2) { /* Simple (double) addition */
    std::cout<<"long double addition\n";
    result=accumulate<long double>(ld);
  }
  else if (s==3) {
    std::cout<<"Ben's extended-exponent addition\n";
    result=accumulate<le_double_t>(cd);
  }
  else if (s==4) {
    std::cout<<"Ben's logarithmic addition\n";
    result=accumulate<log_double_t>(bd);
  }
  else
    std::cout<<"Ben's infinitely fast wrong answer :)\n";


  std::cout<<"result = "<<result<<std::endl;
  return 0;
}

