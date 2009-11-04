/*
   Copyright (C) 2005 Benjamin Redelings

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

#include "colors.H"
#include "util.H"
#include <iostream>

namespace colors {

  using std::vector;
  using std::string;

  //  returns a number in [0,1)
  double wrap(double x) {
    if (x>0)
      return x-(int)x;
    else
      return -x+(int)x;
  }

  string RGB::to_css() const {
    assert(0 <= R() and R() <= 1);
    assert(0 <= G() and G() <= 1);
    assert(0 <= B() and B() <= 1);

    string style = "rgb(";
    style += convertToString((int)(R()*256)) + ",";
    style += convertToString((int)(G()*256)) + ",";
    style += convertToString((int)(B()*256)) + ")";
    return style;
  }

  HSV RGB::to_HSV() const {
    double min = std::min(R(),G());
    min = std::min(min,B());

    double V = std::max(R(),G());
    V = std::max(V,B());

    
    double delta = V - min;

    // Calculate saturation: saturation is 0 if r, g and b are all 0
    double S=0;
    if (V >0) S = delta / V;


    double H=0;

    if(S>0) {
      int highest = argmax(*this);
      if (highest == 0)
	H =           (1.0/6) * (G() - B()) / delta;
      else if (highest == 1)
        H = (1.0/3) + (1.0/6) * (B() - R()) / delta;
      else if (highest == 2)
	H = (2.0/3) + (1.0/6) * (R() - G()) / delta;

      if (H < 0)
	H += 1.0;
    }
    return HSV(H,S,V);
  }

  RGB::RGB() :vector<double>(3) 
  {}

  RGB::RGB(double r,double g,double b)
    :vector<double>(3) 
  {
    assert(-1 <= r and r <= 2);
    assert(-1 <= g and g <= 2);
    assert(-1 <= b and b <= 2);

    R() = r;
    G() = g;
    B() = b;
  }

  string HSV::to_css() const {
    return to_RGB().to_css();
  }

  HSV::HSV() :vector<double>(3)
  {}

  HSV::HSV(double h,double s,double v)
    :vector<double>(3) 
  {
    H() = wrap(h);
    S() = s;
    V() = v;
  }




  RGB HSV::to_RGB() const {
    assert(0 <= H() and H() <= 1);
    assert(0 <= S() and S() <= 1);
    assert(0 <= V() and V() <= 1);
    
    // decompose color range [0,6) into a discrete color (i) and fraction (f)
    double h = H() * 6;
    assert(0 <= h and h < 6);
    int i = (int)h;
    assert(0 <= i and i < 6);
    double f = h-i;
    assert(0 <= f and f < 1);

    double p = V()*(1-S());
    double q = V()*(1-(S()*f));
    double t = V()*(1 - (S() * (1-f)));

    if (i==0) 
      return RGB(V(),t,p);
    else if (i==1) 
      return RGB(q,V(),p);
    else if (i==2) 
      return RGB(p,V(),t);
    else if (i==3) 
      return RGB(p,q,V());
    else if (i==4) 
      return RGB(t,p,V());
    else if (i==5) 
      return RGB(V(),p,q);

    std::abort();
  }

  // this is the OLD whitening function
  RGB whiten1(const RGB& rgb,double p) {
    RGB W=rgb;
    W.R() = (1.0-p)*W.R() + p;
    W.G() = (1.0-p)*W.G() + p;
    W.B() = (1.0-p)*W.B() + p;
    return W;
  }

  /// Whiten color @rgb by fraction p.  w[p]*w[q] = w[1-(1-p)*(1-q)]
  RGB whiten(const RGB& rgb,double p) {
    HSV W1 = rgb;
    HSV W2 = W1;

    W2.S() = (1.0-p)*W1.S();
    W2.V() = 1.0-(1.0-p)*(1.0-W1.V());

    assert(W2.S() <= W1.S());
    assert(W2.V() >= W1.V());
    return W2;
  }


  RGB black    (0, 0, 0);
  RGB white    (1, 1, 1);
  RGB grey   (0.8, 0.8, 0.8);
  RGB yellow (0.9, 0.9, 0.1);
  RGB blue   (0.1, 0.1, 0.9);
  RGB orange (0.9, 0.7, 0.3);
  RGB red    (0.9, 0.1, 0.1);
  RGB green  (0.1, 0.9, 0.1);
  RGB purple (0.9, 0.1, 0.9);

  double grayscale(const RGB& c) {
    return 0.3*c.R() + 0.59*c.G() + 0.11*c.B();
  }


}

