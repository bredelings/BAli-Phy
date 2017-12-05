#include "statistics.H"

#include <iostream>
#include <boost/program_options.hpp>
#include "matrix.H"

namespace po = boost::program_options;
using po::variables_map;

using namespace std;
using namespace statistics;

void report_distances(const valarray<double>& distances,
		      const string& name,
		      variables_map& args
		      )
{
  if (not distances.size()) return;

  bool show_mean = args.count("mean");
  bool show_median = args.count("median");
  bool show_minmax = args.count("minmax");

  if (not show_mean and not show_median and not show_minmax)
    show_median = true;

  if (show_minmax)
    cout<<"    "<<name<<" in ["<<min(distances)<<", "<<max(distances)<<"]"<<endl;
  if (show_mean){
      cout<<"  E "<<name<<" = "<<distances.sum()/distances.size();
      cout<<"   [+- "<<sqrt(Var(distances))<<"]"<<endl;
  }
  if (show_median) {
    double P = args["CI"].as<double>();
    pair<double,double> interval = central_confidence_interval(distances,P);
    cout<<"    "<<name<<" ~ "<<median(distances);
    cout<<"   ("<<interval.first<<", "<<interval.second<<")"<<endl;
  }
}

// We consider 4 random distributions:
//  1. D(t[i],t[j])
//  2. E_t[i] D(t[i],t[j])
//  3. E_t[j] D(t[i],t[j])
//  4. E_t[i],t[j] D(t[i],t[j])
//
//  If t[i] and t[j] have the same distribution then #2 == #3.
//  Also, #4 is constant so it is not really a distribution.

void diameter(const matrix<double>& D,const string& name,variables_map& args)
{
  if (D.size1() == 1)
  {
    cout<<"Group "<<name<<" has only 1 item."<<endl;
    return;
  }

  const unsigned N = D.size1();

  int k=0;
  valarray<double> d1(0.0, N);
  valarray<double> d11(0.0, N*(N-1)/2);

  for(int i=0;i<N;i++)
    for(int j=0;j<i;j++) {
      d1[i] += D(i,j);
      d1[j] += D(i,j);
      d11[k++] = D(i,j);
    }
  d1 /= (N-1);


  string name1 = string("D")+name+name + "  ";
  string name2 = string("D")+name+"("+name+")";
  report_distances(d11,name1, args);cout<<endl;
  report_distances(d1 ,name2, args);
  cout<<endl;
}

double fair_probability_x_less_than_y(const valarray<double>& x,const valarray<double>& y)
{
  return 0.5*(probability_x_less_than_y(x,y) + 1.0 - probability_x_less_than_y(y,x));
}


void report_compare(variables_map& args, const matrix<double>& D, int N1, int N2)
{
  assert(N1+N2 == D.size1());
  assert(D.size1() == D.size2());

  matrix<double> D1(N1,N1);
  for(int i=0;i<N1;i++)
    for(int j=0;j<N1;j++)
      D1(i,j) = D(i,j);

  matrix<double> D2(N2,N2);
  for(int i=0;i<N2;i++)
    for(int j=0;j<N2;j++)
      D2(i,j) = D(N1+i,N1+j);
    
  
  valarray<double> d1(0.0, N1);
  valarray<double> d11(0.0, N1*(N1-1)/2);
      
  {
    int k=0;
    for(int i=0;i<N1;i++)
      for(int j=0;j<i;j++) {
	d1[i] += D1(i,j);
	d1[j] += D1(i,j);
	d11[k++] = D1(i,j);
      }
    d1 /= (N1-1);
  }

  valarray<double> d2(0.0, N2);
  valarray<double> d22(0.0, N2*(N2-1)/2);
      
  {
    int k=0;
    for(int i=0;i<N2;i++)
      for(int j=0;j<i;j++) {
	d2[i] += D2(i,j);
	d2[j] += D2(i,j);
	d22[k++] = D2(i,j);
      }
    d2 /= (N2-1);
  }
  
  cout<<endl;
  diameter(D1,"1",args);

  cout<<endl;
  diameter(D2,"2",args);cout<<endl;

  valarray<double> d12(0.0, N1*N2);
  valarray<double> d12_1(0.0, N1);
  valarray<double> d12_2(0.0, N2);
  for(int i=0;i<N1;i++)
    for(int j=0;j<N2;j++) {
      double DIJ = D(i,N1+j);
      d12[i*N2+j] = DIJ;
      d12_1[i] += DIJ;
      d12_2[j] += DIJ;
    }
  
  d12_1 /= N2;
  d12_2 /= N1;
  
  report_distances(d12,"D12  ",args);cout<<endl;
  if (N2 > 1) {
    report_distances(d12_1 ,"D1(2)",args);cout<<endl;
  }
  if (N1 > 1)
  {
    report_distances(d12_2 ,"D2(1)",args);cout<<endl;
  }
  
  //NOTE: D12 != D11 when 1==2 because D12 includes the zero's on the diagonal.
  
  if (N1 > 1)
    cout<<"    P(D12 > D11) = "<<fair_probability_x_less_than_y(d11,d12)<<endl;
  if (N2 > 1)
    cout<<"    P(D12 > D22) = "<<fair_probability_x_less_than_y(d22,d12)<<endl;
  if (N1 > 1 or N2 > 1)
    cout<<endl;
  if (N1 > 1)
    cout<<"    P(D2(1) > D1(1)) = "<<fair_probability_x_less_than_y(d1,d12_2)<<endl;
  if (N2 > 1)
    cout<<"    P(D1(2) > D2(2)) = "<<fair_probability_x_less_than_y(d2,d12_1)<<endl;
  if (N1 > 1 or N2 > 1)
    cout<<endl;
  if (N1 > 1)
    cout<<"    P(D1(2) > D1(1)) = "<<fair_probability_x_less_than_y(d1,d12_1)<<endl;
  if (N2 > 1)
    cout<<"    P(D2(1) > D2(2)) = "<<fair_probability_x_less_than_y(d2,d12_2)<<endl;
}


