#ifndef COLORS_H
#define COLORS_H
#include <vector>
#include <string>

//TODO - make a unified color class, which can be accessed through
//       BOTH RGB and HSV;

namespace colors {

  class HSV;

  class RGB:public std::vector<double> {
  public:
    double  R() const {return (*this)[0];}
    double& R()       {return (*this)[0];}

    double  G() const {return (*this)[1];}
    double& G()       {return (*this)[1];}

    double  B() const {return (*this)[2];}
    double& B()       {return (*this)[2];}

    HSV to_HSV() const;

    std::string to_css() const;

    operator HSV() const;

    RGB();

    RGB(double r,double g,double b);
  };

  class HSV:public std::vector<double> 
  {
  public:
    double  H() const {return (*this)[0];}
    double& H()       {return (*this)[0];}
    double  S() const {return (*this)[1];}
    double& S()       {return (*this)[1];}
    double  V() const {return (*this)[2];}
    double& V()       {return (*this)[2];}

    RGB to_RGB() const;

    std::string to_css() const;

    operator RGB() const {return to_RGB();}

    HSV();

    HSV(double h,double s,double v);
  };

  inline RGB::operator HSV() const {return to_HSV();}

  RGB whiten(const RGB&,double p);

  extern RGB black;
  extern RGB white;
  extern RGB grey;
  extern RGB yellow;
  extern RGB blue;
  extern RGB orange;
  extern RGB red;
  extern RGB green;
  extern RGB purple;
}
#endif
