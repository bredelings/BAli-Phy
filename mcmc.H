#ifndef MCMC_H
#define MCMC_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"
#include <vector>

class MCMC {

  typedef void (*move)(alignment&,Parameters&);

  struct move_info {
    move m;
    double weight;
    bool enabled;
    vector<string> attributes;

    move_info() {}
    move_info(move a1,double a2,bool a3):m(a1),weight(a2),enabled(a3) {}
  };

  double sum_enabled_weights;

  void recalc();
  double(&probability)(const alignment&,const Parameters&);

public:
  std::vector<move_info> moves;

  void enable(const string&);
  void disable(const string&);
  void add(move,double,const string&);

  void iterate(alignment&,Parameters&,int);
  void sample(alignment&,Parameters&) const;
  void add(move,double);

  MCMC();
};

#endif
