/*
   Copyright (C) 2004-2005 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include <cmath>
#include <limits>
#include "util/assert.hh"
#include <iostream>
#include <valarray>
#include <vector>
#include "optimize.H"
#include "util/myexception.H"
#include "util/io/optional.H"
#include "util/log-level.H"

using std::vector;
using std::cerr;
using std::endl;
using std::ostream;
using std::abs;

namespace optimize {

    Vector search_basis(const Vector& start, const function& f, double delta, int maxiterations)
    {
        const int n = start.size();

        auto v = start;

        vector<double> step_size(n,1);
        vector<int> counter(n,0);

        auto initial_value = f(start);
        if (not initial_value)
            throw myexception()<<"search_basis: initial value outside bounds!";
        double value = *initial_value;

        bool done = false;
        for(int iterations=0; iterations < maxiterations and not done; iterations++)
        {
            // Look for uphill moves in each direction.
            for(int i=0;i<n;i++)
            {
                bool success = false;
                for(int dir:{-1,+1})
                {
                    Vector nextv = v;
                    nextv[i] += step_size[i] * dir;

                    auto next_value = f(nextv);

                    if (log_verbose >= 2)
                    {
                        cerr<<"iteration = "<<iterations<<
                            "   i = "<<i<<
                            "   step_size = "<<step_size[i]<<
                            "   old = "<<value<<
                            "   new = "<<next_value<<endl;
                    }

                    if (next_value and *next_value > value)
                    {
                        v = nextv;
                        value = *next_value;
                        success = true;
                        counter[i]++;
                        break;
                    }

                }

                // If neither direction improves, decrease step size.
                if (not success)
                {
                    counter[i] = 0;
                    step_size[i] /= 2;
                }

                // If a direction increases twice in a row, then double the step size.
                if (counter[i] >= 2)
                {
                    step_size[i] *= 2;
                    counter[i] = 0;
                }
            }
      
            // Are we done yet?
            done = true;
            for(int i=0; i<n; i++)
                if (step_size[i] > delta)
                    done = false;
        }
        if (not done)
            cerr<<"Convergence failed!\n";
        else if (log_verbose)
            cerr<<"search: FINAL = "<<f(v)<<"\n";

        return v;
    }

  void getlimits(const Vector& x,const Vector& v,double& min,double& max) {
    assert(x.size() == v.size());

    min = -std::numeric_limits<double>::infinity();
    max = std::numeric_limits<double>::infinity();

    for(int i=0;i<x.size();i++) {
      if (v[i] < 0)
	max = std::min(max,-x[i]/v[i]);
      else if (v[i] > 0)
	min = std::max(min,-x[i]/v[i]);
    }
  }


  /// Calculate the first derivate of f in the direction dx
  double derivative(const function& f,const Vector& x,
		    const Vector& v,const double dt) 
  {
    assert(x.size() == v.size());

    // Use a centered derivative, if possible
    double t1 = -0.5*dt;
    double t2 = 0.5*dt;

    double deltat = t2 - t1;
    
    // Calculate the derivative at [x + v*(
    double f1 = *f(x + t1 * v);
    double f2 = *f(x + t2 * v);

#ifndef NDEBUG
    //    cerr<<" f1 = "<<f1<<"    f2 = "<<f2<<"\n";
#endif

    return (f2-f1)/deltat;
  }

  /// Calculate the second derivate of f in the direction dx
  double derivative2(const function& f,const Vector& x,
		     const Vector& v,const double dt) {

    assert(x.size() == v.size());

    // Use a centered derivative, if possible
    double t1 = -dt;
    double t3 = dt;
    
    double deltat = (t3 - t1)/2;
    double t2 = (t1+t3)/2;

    // Calculate the 2-derivative at nx + dt*dx_pos
    double f1 = *f(x + t1*v);
    double f2 = *f(x + t2*v);
    double f3 = *f(x + t3*v);

    return (f3 - f2*2 + f1)/(deltat*deltat);
  }


  /// Calculate the gradient of the function f at x, w/ dx as the vector of delta-x's
  Vector gradient(const function& f,const Vector& x,const Vector& dx) {
    Vector g(x.size());

    assert(x.size() == dx.size());

    Vector x1(x.size());
    Vector x2(x.size());

    for(int i=0;i<x.size();i++) 
    {
      double dxi = abs(dx[i]);

      x1 = x;
      x2 = x;

      x1[i] -= dxi*0.5;
      x2[i] += dxi*0.5;

      g[i] = (*f(x2) - *f(x1))/dxi;
    }

    return g;
  }


  Vector Proj(const Vector& x) {
    return x;

    Vector x2 = x;
    for(int i=0;i<x2.size();i++)
      if (x2[i] < 0) x2[i] = 0;

    return x2;
  }

  Vector Proj(const Vector& v, const Vector& x) {
    return v;

    assert(v.size() == x.size());
    Vector v2 = v;
    for(int i=0;i<x.size();i++)
      if (x[i] <= 0 and v2[i] < 0) v2[i] = 0;

    return v2;
  }


  /// FIXME - we should probably do a more general lower/upper bound thingy
  ///  But how to specify the BOUNDS on the model parameter bounds?
  //   Probably have to add that to the model...

  /// FIXME - This could be improved to do a search in the neigborhood of the best point found

  /// Do a line search from x to x+direction for maximizing function f...
  bool line_search(Vector& x,const function& f,const Vector& direction,int ntries) {

    const double value1 = *f(x);
    const Vector x1 = x;

    // Start line search
    bool moved = false;
    double scale = 1.0;
    double cvalue = value1;
    for(int i=0;i<ntries;i++) {

      Vector x2a = Proj(x1 + direction*scale);
      Vector x2b = Proj(x1 + direction/scale);
      double value2a = *f(x2a);
      double value2b = *f(x2b);

      Vector x2 = x2a;
      double value2 = value2a;
      if (value2b > value2) {
	x2 = x2b;
	value2 = value2b;
      }

#ifndef NDEBUG
      cerr<<"  retry #"<<i<<"  value: "<<cvalue<<" -> "<<value2;
#endif

      if (value2 > cvalue and not std::isnan(value2)) {
#ifndef NDEBUG
	cerr<<" [ACCEPTED]\n";
#endif
	x = x2;
	cvalue = value2;
	moved = true;
      }
      else {
#ifndef NDEBUG
	cerr<<" [REJECTED]\n";
#endif

	// Bail on the first bad proposal after a good proposal
	if (moved) break;
      }
	
      
      scale *= 0.6;
    }
    
    return moved;
  }

  double infnorm(const Vector& x) {
    double d=0;
    for(int i=0;i<x.size();i++)
      if (abs(x[i]) > d)
	d = abs(x[i]);
    return d;
  }

  ostream& operator<<(ostream& o,const Vector& x) {
    int p = o.precision();
    o.precision(4);
    for(int i=0;i<x.size();i++) {
      o<<x[i];
      if (i != x.size()-1)
	o<<" ";
    }
    o.precision(p);
    return o;
  }


  // How are we going to deal w/ boundaries?

  // Also - we're running into precision limits on D2 - we need to use a larger delta...

  // We can get away with a smaller one for D1

  Vector search_gradient(const Vector& start,const function& f, 
			 double delta,int maxiterations) 
  {
    Vector x = start;
    Vector dx(1.0e-5,start.size());
    double value = *f(x);
    double df = 1.0;

    bool done = false;
    for(int iteration=0;iteration<maxiterations and not done;iteration++) 
    {
      cerr<<"\niteration = "<<iteration<<"   f = "<<value<<endl;

      cerr<<" df = "<<df<<" [target]\n";

      cerr<<" x = "<<x<<"\n";

      cerr<<" dx = "<<dx<<"\n";

      //----------------- Calculate the gradient ----------------------//
      Vector del_f = Proj(gradient(f,x,dx),x);
      double lambda = df/dot(del_f,del_f);
      Vector dx2 = del_f*lambda;
      
      cerr<<" del_f = "<<del_f<<"\n";

      cerr<<" dx2 = "<<dx2<<"\n";

      //------------ Estimate Distance using Newton-Raphson -----------//

      cerr<<"\nNewton/Raphson...\n";

      // we have a path x+t*dx2
      double D1 = derivative (f,x,dx2,0.01);
      double D2 = derivative2(f,x,dx2,0.05);
      cerr<<" D1 = "<<D1<<"     D2 = "<<D2<<"\n";

      double t = -D1/D2;

      Vector NR_dx = t*dx2; // Newton-Raphson estimate

      cerr<<" NR df = "<<D1*t<<" [linear prediction]\n";
      cerr<<" NR df = "<<-D1*D1/(2.0*D2)<<" [quadratic prediction]\n";

      cerr<<" NR dx = "<<NR_dx<<" [proposed]\n\n";
      cerr<<" NR x = "<<x + NR_dx<<" [proposed]\n\n";


      //------------------------- Do line search ----------------------//
      bool moved = false;
      const Vector x1 = x;
      cerr<<"\nNewton/Raphson...\n";
      if (t > 0) {
	moved = line_search(x,f,NR_dx,5);
	if (moved)
	  cerr<<" NR succeeded: x = "<<x<<"\n";
	else
	  cerr<<" NR failed.\n";
      }
      else
	cerr<<" ignored: not convex.\n";

      if (not moved) {
	cerr<<"\nREGULAR...\n";
	if (line_search(x,f,dx2,15)) {
	  cerr<<" REGULAR succeeded: x = "<<x<<"\n";
	  moved = true;
	}
	else
	  cerr<<" REGULAR failed.\n";
      }

      //------------ Update State based on moved/not moved --------------//
      if (moved) {
	double value2 = *f(x);
	df = value2-value;
	value = value2;
	Vector deltax = (x - x1)/10;
	for(int i=0;i<deltax.size();i++)
	  if (deltax[i] > 1.0e-9*(1.0+abs(x[i])))
	    dx[i] = deltax[i];
      }
      else {
	Vector deltax = dx/2.0;
	for(int i=0;i<deltax.size();i++)
	  if (deltax[i] > 1.0e-9*(1.0+abs(x[i])))
	    dx[i] = deltax[i];
	df *= 0.5;
      }

      //------------------------- Are we done yet ----------------------//
      if (infnorm(NR_dx) < delta*(1.0+infnorm(x)) 
	  and infnorm(del_f) < delta*abs(1.0+value)) {
	done = true;
	cerr<<"not done:  ||dx||/(1+||x||) = "<<infnorm(NR_dx)/(1+infnorm(x))<<"         ";
	cerr<<"||del fx||/(1+||f||) = "<<infnorm(del_f)/(1+abs(1.0+value))<<"\n";
	cerr<<"GRADIENT: final = "<<value<<"       iterations = "<<iteration<<"\n";
      }
      else {
	cerr<<"not done:  ||dx||/(1+||x||) = "<<infnorm(NR_dx)/(1+infnorm(x))<<"         ";
	cerr<<"||del fx||/(1+||f||) = "<<infnorm(del_f)/(1+abs(1.0+value))<<"\n";
      }
    }
    return x;
  }

}
