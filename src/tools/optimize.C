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
    std::valarray<bool> moved(false,2*dimension);
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
	if (ii == basis.size())
	  nextv += last1 + last2;
	else
	  nextv[ii%dimension] += basis[ii];

	double next_value = f(nextv);

#ifndef NDEBUG
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
#endif

	// If we moved, update last1, last2
	if (next_value > value) {
	  last2 = last1;
	  last1 = (nextv - v);
	}

	// If this is one of the basis directions
	//  then update scale for that direction
	if (ii < 2*dimension) {
	  bool second_time = moved[ii];
	  moved = false;
	  if (next_value > value) {
	    if (second_time)
	      basis[ii] *= 2.0;
	    else 
	      moved[ii] = true;
	  }
	  else 
	    basis[ii] /= 2.0;
	}

	// Do the move, if we're better
	if (next_value > value) {
#ifndef NDEBUG
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
#endif

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
#ifndef NDEBUG
      if (done)
	std::cerr<<"BASIS: final = "<<value<<"       iterations = "<<iterations<<"\n";
#endif
    }
    if (not done)
      std::cerr<<"Convergence failed!\n";

    return v;
  }

  void getlimits(const Vector& x,const Vector& v,double& min,double& max) {
    assert(x.size() == v.size());

    min = -1.0/0;
    max = 1.0/0;

    for(int i=0;i<x.size();i++) {
      if (v[i] < 0)
	max = std::min(max,-x[i]/v[i]);
      else if (v[i] > 0)
	min = std::max(min,-x[i]/v[i]);
    }
  }


  /// Calculate the first derivate of f in the direction dx
  double derivative(const function& f,const Vector& x,
		    const Vector& v,const double dt) {

    assert(x.size() == v.size());

    double min,max;
    getlimits(x,v,min,max);


#ifndef NDEBUG
    std::cerr<<" min = "<<min<<"   max = "<<max<<"\n";
#endif
    // Use a centered derivative, if possible
    double t1 = -0.5*dt;
    double t2 = 0.5*dt;
    
    // But adjust it to fit
    if (min > t1) 
      t1 = min * 0.99;
    if (max < t2)
      t2 = max * 0.99;
    double deltat = t2 - t1;
    
    std::cerr<<"  t1 = "<<t1<<"    t2 = "<<t2<<"\n";

    // Calculate the derivative at [x + v*(
    double f1 = f(x + t1 * v);
    double f2 = f(x + t2 * v);

#ifndef NDEBUG
    std::cerr<<" f1 = "<<f1<<"    f2 = "<<f2<<"\n";
#endif

    return (f2-f1)/deltat;
  }

  /// Calculate the second derivate of f in the direction dx
  double derivative2(const function& f,const Vector& x,
		     const Vector& v,const double dt) {

    assert(x.size() == v.size());

    double min,max;
    getlimits(x,v,min,max);

    // Use a centered derivative, if possible
    double t1 = -dt;
    double t3 = dt;
    
    // But adjust it to fit
    if (min > t1) 
      t1 = min * 0.99;
    if (max < t3)
      t3 = max * 0.99;

    double deltat = t3 - t1;
    double t2 = (t1+t3)/2;

    // Calculate the 2-derivative at nx + dt*dx_pos
    double f1 = f(x + t1*v);
    double f2 = f(x + t2*v);
    double f3 = f(x + t3*v);

    return (f3 - f2*2 + f1)/(deltat*deltat);
  }


  /// Calculate the gradient of the function f at x, w/ dx as the vector of delta-x's
  Vector gradient(const function& f,const Vector& x,const Vector& dx) {
    Vector g(x.size());

    assert(x.size() == dx.size());

    for(int i=0;i<x.size();i++) {
      double f1 = f(x);

      Vector nx = x;
      double dxi = std::abs(dx[i]);
      nx[i] += dxi;
      double f2 = f(nx);
      g[i] = (f2-f1)/dxi;
    }

    return g;
  }


  Vector Proj(const Vector& x) {
    Vector x2 = x;
    for(int i=0;i<x2.size();i++)
      if (x2[i] < 0) x2[i] = 0;

    return x2;
  }

  Vector Proj(const Vector& v, const Vector& x) {
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

    const double value1 = f(x);
    const Vector x1 = x;

    // Start line search
    bool moved = false;
    double scale = 1.0;
    double cvalue = value1;
    for(int i=0;i<ntries;i++) {

      Vector x2a = Proj(x1 + direction*scale);
      Vector x2b = Proj(x1 + direction/scale);
      double value2a = f(x2a);
      double value2b = f(x2b);

      Vector x2 = x2a;
      double value2 = value2a;
      if (value2b > value2) {
	x2 = x2b;
	value2 = value2b;
      }

#ifndef NDEBUG
      std::cerr<<"  retry = "<<i<<"      value = "<<value2<<std::endl;
#endif

      if (value2 > cvalue and not isnan(value2)) {
#ifndef NDEBUG
	std::cerr<<"move: "<<value2<<" > "<<cvalue<<"\n";
#endif
	x = x2;
	cvalue = value2;
	moved=true;
      }
      else if (moved)
	break;
	
      
      scale *= 0.6;
    }
    
    return moved;
  }

  double infnorm(const Vector& x) {
    double d=0;
    for(int i=0;i<x.size();i++)
      if (std::abs(x[i]) > d)
	d = std::abs(x[i]);
    return d;
  }


  // How are we going to deal w/ boundaries?

  // Also - we're running into precision limitsf on D2 - we need to use a larger delta...

  // We can get away with a smaller one for D1

  Vector search_gradient(const Vector& start,const function& f, 
				 double delta,int maxiterations) 
  {
    Vector x = start;
    Vector dx(1.0e-5,start.size());
    double value = f(x);
    double df = 1.0;

    bool done = false;
    for(int iteration=0;iteration<maxiterations and not done;iteration++) {
      std::cerr<<"iteration = "<<iteration<<"   f = "<<value<<std::endl;

      std::cerr<<"x = ";
      for(int i=0;i<x.size();i++)
	std::cerr<<x[i]<<" ";
      std::cerr<<"\n\n";

      std::cerr<<"dx = ";
      for(int i=0;i<dx.size();i++)
	std::cerr<<dx[i]<<" ";
      std::cerr<<"\n\n";

      /*----------------- Calculate the gradient ----------------------*/
      Vector del_f = Proj(gradient(f,x,dx),x);
      double lambda = df/dot(del_f,del_f);
      Vector dx2 = del_f*lambda;
      
      std::cerr<<"del_f = ";
      for(int i=0;i<del_f.size();i++)
	std::cerr<<del_f[i]<<" ";
      std::cerr<<"\n\n";

      std::cerr<<"dx2 = ";
      for(int i=0;i<dx2.size();i++)
	std::cerr<<dx2[i]<<" ";
      std::cerr<<"\n\n";

      /*--------------- Estimate Distance using Newton-Raphson -------------*/

      // we have a path x+t*dx2
      double D1 = derivative(f,x,dx2,0.125);
      double D2 = derivative2(f,x,dx2,0.25);

      double t = -D1/D2;

      Vector dxNR = t*dx2; // Newton-Raphson estimate

      std::cerr<<"df = "<<df<<"\n";
      std::cerr<<"D1 = "<<D1<<"     D2 = "<<D2<<"  !\n";

      D1 = derivative(f,x,dx2,0.125);
      D2 = derivative2(f,x,dx2,0.125);
      std::cerr<<"D1 = "<<D1<<"     D2 = "<<D2<<"  !\n";

      D1 = derivative(f,x,dx2,0.25);
      D2 = derivative2(f,x,dx2,0.25);
      std::cerr<<"D1 = "<<D1<<"     D2 = "<<D2<<"  !\n";

      std::cerr<<"NR predicted df = "<<dot(del_f,dxNR)<<"\n";

      std::cerr<<"dxNR = ";
      for(int i=0;i<dxNR.size();i++)
	std::cerr<<dxNR[i]<<" ";
      std::cerr<<"\n\n";


      /*------------------------- Do line search ----------------------*/

      bool moved = false;
      const Vector x1 = x;
      if (t > 0) {
	moved = line_search(x,f,dxNR,5);
	if (moved)
	  std::cerr<<"NR succeeded...\n";
	else
	  std::cerr<<"NR failed...\n";
      }

      std::cerr<<"x = ";
      for(int i=0;i<x.size();i++)
	std::cerr<<x[i]<<" ";
      std::cerr<<"\n\n";

       
      if (line_search(x,f,dx2,15)) {
	  std::cerr<<"REGULAR succeeded...\n";
	  moved = true;
      }
      else
	std::cerr<<"REGULAR failed...\n";

      std::cerr<<"x = ";
      for(int i=0;i<x.size();i++)
	std::cerr<<x[i]<<" ";
      std::cerr<<"\n\n";


      /*------------ Update State based on moved/not moved --------------*/
      if (moved) {
	double value2 = f(x);
	df = value2-value;
	value = value2;
	Vector deltax = (x - x1)/10;
	for(int i=0;i<deltax.size();i++)
	  if (deltax[i] > 1.0e-9*(1.0+std::abs(x[i])))
	    dx[i] = deltax[i];
      }
      else {
	Vector deltax = dx/2.0;
	for(int i=0;i<deltax.size();i++)
	  if (deltax[i] > 1.0e-9*(1.0+std::abs(x[i])))
	    dx[i] = deltax[i];
	df *= 0.5;
      }

      /*------------------------- Are we done yet ----------------------*/
      if (infnorm(dxNR) < delta*(1.0+infnorm(x)) 
	  and infnorm(del_f) < delta*std::abs(1.0+value)) {
	done = true;
	std::cerr<<"not done:  ||dx||/(1+||x||) = "<<infnorm(dxNR)/(1+infnorm(x))<<"         ";
	std::cerr<<"||del fx||/(1+||f||) = "<<infnorm(del_f)/(1+std::abs(1.0+value))<<"\n";
	std::cerr<<"GRADIENT: final = "<<value<<"       iterations = "<<iteration<<"\n";
      }
      else {
	std::cerr<<"not done:  ||dx||/(1+||x||) = "<<infnorm(dxNR)/(1+infnorm(x))<<"         ";
	std::cerr<<"||del fx||/(1+||f||) = "<<infnorm(del_f)/(1+std::abs(1.0+value))<<"\n";
      }
    }
    return x;
  }
}
