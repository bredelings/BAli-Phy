#include <cassert>
#include <iostream>
#include <valarray>
#include "optimize.H"

namespace optimize {

  Vector search_basis(const Vector& start,const function& f, double delta,int maxiterations) {
    const int dimension = start.size();

    Vector v = start;

    Vector last1(0.0,start.size());
    Vector last2(0.0,start.size());

    // vector of sizes for each direction
    std::vector<bool> moved(2*dimension,false);
    std::vector<double> basis(2*dimension,1);
    for(int i=dimension;i<basis.size();i++)
      basis[i] = -1;

    // where we left off last time
    int k=0;
    bool done = false;

    const int n = basis.size()+1;

    double value = f(start);
    for(int iterations=0;iterations<maxiterations and not done;iterations++) {
      for(int i=0;i<n;i++) {
	int ii = (i+k)%n;

	// Propose the next point
	Vector nextv = v;
	if (ii == basis.size()) {
	  for(int j=0;j<nextv.size();j++)
	    nextv[j] += last1[j] + last2[j];
	}
	else
	  nextv[ii%dimension] += basis[ii];


	double next_value = f(nextv);

	// Show some current status
	if (ii == basis.size()) {
	  std::cerr<<"iteration = "<<iterations<<
	    "   ii = "<<ii<<
	    "   v = ";
	  for(int j=0;j<nextv.size();j++)
	    std::cerr<<last1[j] + last2[j]<<" ";
	  std::cerr<<
	    "\n   old = "<<value<<
	    "   new = "<<next_value<<std::endl;
	  
	}
	else {
	  std::cerr<<"iteration = "<<iterations<<
	    "   ii = "<<ii<<
	    "   size = "<<basis[ii]<<
	    "   old = "<<value<<
	    "   new = "<<next_value<<std::endl;
	}
	break;

	// Alter some state
	if (ii == basis.size()) {
	  if (next_value > value) {
	    last2 = last1;
	    for(int j=0;j<nextv.size();j++)
	      last1[j] += last2[j];
	  }
	}
	else {
	  if (next_value > value) {
	    last2 = last1;

	    last1 = Vector(0.0,last1.size());
	    last1[ii] = basis[ii];

	    if (moved[ii]) {
	      basis[ii] *= 2.0;
	      moved[ii] = false;
	    }
	    else
	      moved[ii] = true;
	  }
	  else {
	    moved[ii] = false;
	    basis[ii] /= 2.0;
	  }
	}

	// Do the move, if we're better
	if (next_value > value) {
	  std::cerr<<"Moved from  old = "<<value<<
	    " to new = "<<next_value<<std::endl;

	  // Display the current position
	  for(int j=0;j<v.size();j++)
	    std::cerr<<v[j]<<"  ";
	  std::cerr<<std::endl;

	  // Display the next position
	  for(int j=0;j<nextv.size();j++)
	    std::cerr<<nextv[j]<<"  ";
	  std::cerr<<std::endl;

	  v = nextv;
	  value = next_value;
	  k = ii+1;

	  break;
	}
      } // Do this n times
      
      // Are we done yet?
      done = true;
      for(int i=0;i<basis.size();i++)
	if (std::abs(basis[i]) > delta) done = false;
    }
    if (not done)
      std::cerr<<"Convergence failed!\n";

    return v;
  }

  double derivative(const function& f,const Vector& x,
		    Vector dx,const double scale) {
    dx *= (scale*0.5);
    assert(x.size() == dx.size());

    double f1 = f(x-dx);
    double f2 = f(x+dx);

    return (f2-f1)/scale;
  }

  double derivative2(const function& f,const Vector& x,
		     Vector dx,const double scale) {
    dx *= (scale*0.5);
    double f1 = f(x-dx);
    double f2 = f(x);
    double f3 = f(x+dx);

    return (f3 - (2*f2) + f1)/(0.25*scale*scale);
  }


  
  Vector gradient(const function& f,const Vector& x,const Vector& dx) {
    Vector g(x.size());

    assert(x.size() == dx.size());

    for(int i=0;i<x.size();i++) {
      Vector nx = x; 
      nx[i] -= dx[i]/2.0;
      double f1 = f(nx);

      nx[i] += dx[i];
      double f2 = f(nx);
      g[i] = (f2-f1)/dx[i];
    }

    return g;
  }



  Vector search_gradient(const Vector& start,const function& f, 
				 double delta,int maxiterations) 
  {
    const int ntries = 10;

    Vector x = start;
    Vector dx(1.0e-5,start.size());
    double value = f(x);
    double df = 1.0;

    bool done = false;
    for(int i=0;i<maxiterations and not done;i++) {
      Vector del_f = gradient(f,x,dx);
      double lambda = dot(del_f,del_f)/100.0;
      Vector dx2 = del_f*lambda;
      
      // we have a path x+t*dx2
      double D1 = derivative(f,x,dx2);
      double D2 = derivative2(f,x,dx2);

      double t = -D1/D2;

      bool moved = false;
      Vector dx3 = dx2*t;
      for(int i=0;i<ntries;i++) {
	Vector x2 = x + dx3;
	double value2 = f(x2);
	if (value2 > value) {
	  x = x2;
	  df = value2-value;
	  value = value2;
	  dx = dx3*1.0/100;
	  moved=true;
	  break;
	}
	dx3 *= 0.6;
      }

      if (moved) {
	// Are we done yet?
	done = true;
	for(int i=0;i<dx3.size();i++)
	  if (std::abs(dx3[i]) > delta) done = false;
      }
      else {
	dx *= 0.5;
	df *= 0.5;
      }
      
    }
    return x;
  }
}
