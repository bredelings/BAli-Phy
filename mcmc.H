#ifndef MCMC_H
#define MCMC_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"
#include <map>

namespace mcmc_local {
  struct lstr {
    bool operator()(const std::string& s1, const std::string& s2) const {
      return strcmp(s1.c_str(), s2.c_str()) < 0;
    }
  };
};


class MCMC {

  typedef void (*move)(alignment&,Parameters&);

  struct move_info {
    move m;
    double weight;
    bool enabled;
    move_info() {}
    move_info(move a1,double a2,bool a3):m(a1),weight(a2),enabled(a3) {}
  };

  double sum_enabled_weights;

  move_info& find(const string& s);
  const move_info& find(const string& s) const;

  void recalc();
  double(&probability)(const alignment&,const Parameters&);

public:
  std::map<string,move_info,mcmc_local::lstr> moves;

  bool enabled(const string&) const;
  void enable(const string&);
  void disable(const string&);
  void add(move,double,const string&);

  void iterate(alignment&,Parameters&,int);
  void sample(alignment&,Parameters&) const;
  void add(move,double);

  MCMC();
};

#endif
