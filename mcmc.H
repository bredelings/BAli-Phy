#ifndef MCMC_H
#define MCMC_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"
#include <vector>
#include "rng.H"

// how to have different models, with different moves
// and possibly moves between models?

namespace MCMC {
  enum result_t {success,failure,no_result};

  typedef result_t (*atomic_move)(alignment&,Parameters&);
  typedef result_t (*atomic_move_arg)(alignment&,Parameters&,int);

  // The basic 'Move' class
  class Move {

    bool enabled_;

    // How do we link moves into probability model?
    // double(&probability)(const alignment&,const Parameters&);
    // Could we make a generic MH move here? 
    
  public:
    vector<string> attributes;

    double iterations;
    int failures;
    int successes;

    virtual Move* clone() const =0;

    bool enabled() {return enabled_;}
    void enable() {enabled_=true;}
    void disable() {enabled_=false;}
    
    virtual void enable(const string&);
    virtual void disable(const string&);

    virtual int reset(double) =0;
    virtual void iterate(alignment&,Parameters&) =0;
    virtual result_t iterate(alignment&,Parameters&,int) =0;

    virtual void show_enabled(int depth=0) const;
    virtual void print_move_stats(int depth=0) const;

    Move(const string&);

    virtual ~Move() {}
  };


  class MoveGroupBase {
  protected:
    std::vector<Move*> moves;
    std::vector<double> lambda;
  public:
    int nmoves() const {return moves.size();}
    void add(double,const Move& m);

    MoveGroupBase& operator=(const MoveGroupBase&);
    MoveGroupBase(const MoveGroupBase&);
    MoveGroupBase() {}

    ~MoveGroupBase();
  };

  class MoveGroup: public Move,public MoveGroupBase {
  protected:
    vector<int> order;
    vector<int> suborder;
    
    double sum() const;
    virtual void getorder(double l)=0;
  public:

    MoveGroup* clone() const =0;

    void enable(const string&);
    void disable(const string&);

    int reset(double);
    void iterate(alignment&,Parameters&);
    result_t iterate(alignment&,Parameters&,int);

    void show_enabled(int depth=0) const;
    void print_move_stats(int depth=0) const;

    MoveGroup(const string& s):Move(s) {}

    virtual ~MoveGroup() {}
  };

  class MoveAll: public MoveGroup {
    void getorder(double l);
  public:
    MoveAll* clone() const {return new MoveAll(*this);}

    MoveAll(const string& s):MoveGroup(s) {}
    virtual ~MoveAll() {}
  };

  class MoveOne: public MoveGroup {
    int choose() const;
    void getorder(double l);
  public:
    MoveOne* clone() const {return new MoveOne(*this);}

    MoveOne(const string& s):MoveGroup(s) {}
    virtual ~MoveOne() {}
  };

  class SingleMove: public Move {
    atomic_move m;
  public:
    SingleMove* clone() const {return new SingleMove(*this);}

    int reset(double lambda) {return poisson(lambda);}

    void iterate(alignment& A,Parameters& P) { iterate(A,P,0); }

    result_t iterate(alignment& A,Parameters& P,int) 
    {
#ifndef NDEBUG
      std::cerr<<" [single]move = "<<attributes[0]<<endl;
#endif
      iterations++;
      result_t r = (*m)(A,P);
      if (r == success)
	successes++;
      if (r == failure)
	failures++;
      return r;
    }

    SingleMove(atomic_move m1,const string& v)
      :Move(v),m(m1) {}
  };

  class MoveArg: public Move {
  public:
    MoveArg* clone() const=0;

    vector<int> args;

    MoveArg(const string& s):Move(s) { }

    virtual ~MoveArg() {}
  };


  // can I make this inherit from MoveGroup?
  class MoveEach: public MoveArg, protected MoveGroupBase {
    vector<int> order;

    vector< vector<bool> > present;

    double sum(int) const;
    int choose(int) const;
  public:
    MoveEach* clone() const {return new MoveEach(*this);}

    void add(double l,const MoveArg& m);

    int reset(double);
    void iterate(alignment&,Parameters&);
    result_t iterate(alignment&,Parameters&,int);
    
    void show_enabled(int depth=0) const;
    void print_move_stats(int depth=0) const;

    MoveEach(const string& s):MoveArg(s) {}

    virtual ~MoveEach() {}
  };


  class MoveArgSingle: public MoveArg {
    atomic_move_arg m;
  public:
    MoveArgSingle* clone() const {return new MoveArgSingle(*this);}

    int reset(double l) {return poisson(l);};
    void iterate(alignment& A,Parameters& P) { iterate(A,P,0);};

    result_t iterate(alignment& A,Parameters& P,int a) 
    {
#ifndef NDEBUG
      std::cerr<<" [single]move = "<<attributes[0]<<endl;
#endif
      iterations++;
      result_t r = (*m)(A,P,a);
      if (r == success)
	successes++;
      if (r == failure)
	failures++;
      return r;
    }
    
    MoveArgSingle(const string& s,atomic_move_arg m1,const vector<int>& a)
      :MoveArg(s),m(m1) 
    {args=a;}

    ~MoveArgSingle() {}
  };



  // This makes a Sampler out of a move
  class Sampler: public MoveAll {

    // How do we link moves into probability model?
    double(&probability)(const alignment&,const Parameters&);
  public:
    void go(alignment& A, Parameters& P, int max);

    Sampler(const string& name,double(&p)(const alignment&,const Parameters&)):MoveAll(name),probability(p) {};
  };

}

#endif
