#include "colors.H"
#include "util.H"

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
    int i = (int)h;
    double f = h-i;

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

  RGB black    (0, 0, 0);
  RGB white    (1, 1, 1);
  RGB grey   (0.8, 0.8, 0.8);
  RGB yellow (0.9, 0.9, 0.1);
  RGB blue   (0.1, 0.1, 0.9);
  RGB orange (0.9, 0.7, 0.3);
  RGB red    (0.9, 0.1, 0.1);
  RGB green  (0.1, 0.9, 0.1);
  RGB purple (0.9, 0.1, 0.9);

}

