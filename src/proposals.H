#ifndef PROPOSALS_H
#define PROPOSALS_H

#include <valarray>

double dirichlet_fiddle(std::valarray<double>& p,double N);

double dirichlet_fiddle(std::valarray<double>& p,double N,const std::valarray<bool>& mask);

double dirichlet_fiddle_old(std::valarray<double>& p,double sigma);

double dirichlet_fiddle_old(std::valarray<double>& p,double sigma,const std::valarray<bool>& mask);

double scale_gaussian(double& x,double sigma);

#include "alignment.H"
#include "parameters.H"

class Proposal: public Cloneable {
public:
  Proposal* clone() const =0;
  virtual double operator()(alignment& A, Parameters& P) const=0;
};

typedef double (*generic_proposal)(alignment& A, Parameters& P);

class Generic_Proposal: public Proposal
{
  generic_proposal proposal;
  
public:
  Generic_Proposal* clone() const {return new Generic_Proposal(*this);}
  double operator()(alignment& A, Parameters& P) const {return proposal(A,P);}
  Generic_Proposal(generic_proposal gp):proposal(gp) {}
};

typedef double (*proposal_fn)(std::valarray<double>& x,const std::vector<double>& p);

class Proposal2: public Proposal
{
  proposal_fn proposal;
  std::vector<int> indices;
  std::vector<std::string> pnames;

public:
  Proposal2* clone() const {return new Proposal2(*this);}
  double operator()(alignment& A, Parameters& P) const;
  Proposal2(proposal_fn p,const std::string& s, const std::vector<string>& v,
	    const Parameters& P);
};


double frequency_proposal(alignment& A, Parameters& P);

double scale_gaussian2(std::valarray<double>& x,const std::vector<double>& p);

double shift_gaussian_wrap(std::valarray<double>& x,const std::vector<double>& p);
#endif
