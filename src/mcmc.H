#ifndef MCMC_H
#define MCMC_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"
#include <vector>
#include <valarray>
#include "rng.H"

// how to have different models, with different moves
// and possibly moves between models?

namespace MCMC {

  typedef valarray<double> result_t;

  typedef result_t (*atomic_move)(alignment&,Parameters&);
  typedef result_t (*atomic_move_arg)(alignment&,Parameters&,int);

  /// The basic 'Move' class
  class Move {

    bool enabled_;

    // How do we link moves into probability model?
    // double(&probability)(const alignment&,const Parameters&);
    // Could we make a generic MH move here? 
    
  public:
    vector<string> attributes;

    result_t total_results;

    double iterations;

    /// Make a copy of this object
    virtual Move* clone() const =0;

    /// Is this move enabled?
    bool enabled() {return enabled_;}

    /// Enable this move
    void enable() {enabled_=true;}

    /// Disable this move
    void disable() {enabled_=false;}
    
    /// Enable this move or any submove with attribute 's'
    virtual void enable(const string& s);

    /// Disable this move or any submove with attribute 's'
    virtual void disable(const string& s);

    /// Set up for an iteration of length l, return number of subiterations
    virtual int reset(double l) =0;

    /// Do a complete iteration (a top-level routine)
    virtual void iterate(alignment&,Parameters&) =0;

    /// Do the i-th iteration for this round (not a top-level routine)
    virtual result_t iterate(alignment&,Parameters&,int i) =0;

    /// Show enabled-ness for this move and submoves
    virtual void show_enabled(int depth=0) const;

    /// Show acceptance rates for this move and submoves
    virtual void print_move_stats(int depth=0) const;

    /// construct a new move called 's'
    Move(const string& s);

    virtual ~Move() {}
  };

  // FIXME? We could make this inherit from virtual public Move...
  //    but that seems to introduce problems...
  // We could also move the code to this class, and call it from the classes
  //    that use it.

  /// A collection of moves
  class MoveGroupBase {
  protected:
    /// This list of moves
    std::vector<Move*> moves;

    /// This weight of each move
    std::vector<double> lambda;

  public:
    int nmoves() const {return moves.size();}
    void add(double,const Move& m,bool=true);

    MoveGroupBase& operator=(const MoveGroupBase&);
    MoveGroupBase(const MoveGroupBase&);
    MoveGroupBase() {}

    ~MoveGroupBase();
  };

  /// A Move which runs parts of a number of submoves each round
  class MoveGroup: public Move,public MoveGroupBase {
  protected:

    /// An ordered list of submoves to run this round
    vector<int> order;

    /// suborder[i] is the n-th time we've run order[i]
    vector<int> suborder;
    
    double sum() const;

    /// Setup 'order' and 'suborder' for this round
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

  /// A Move which runs all submoves each round
  class MoveAll: public MoveGroup {
    void getorder(double l);
  public:
    MoveAll* clone() const {return new MoveAll(*this);}

    MoveAll(const string& s):MoveGroup(s) {}
    virtual ~MoveAll() {}
  };

  /// A Move which runs one submoves each round
  class MoveOne: public MoveGroup {
    int choose() const;
    void getorder(double l);
  public:
    MoveOne* clone() const {return new MoveOne(*this);}

    MoveOne(const string& s):MoveGroup(s) {}
    virtual ~MoveOne() {}
  };

  /// A Move which runs a specific moves each round
  class SingleMove: public Move {
    atomic_move m;
  public:
    SingleMove* clone() const {return new SingleMove(*this);}

    int reset(double lambda);

    void iterate(alignment& A,Parameters& P) { iterate(A,P,0); }

    result_t iterate(alignment& A,Parameters& P,int);

    SingleMove(atomic_move m1,const string& v)
      :Move(v),m(m1) {}
  };

  /// A move which takes an integer argument from a supplied list
  class MoveArg: public Move {
    /// The ordered list of args to operate on this round
    vector<int> order;

  public:
    MoveArg* clone() const=0;

    /// A list of arguments to be passed to submoves
    vector<int> args;

    int reset(double);
    void iterate(alignment&,Parameters&);
    result_t iterate(alignment&,Parameters&,int);

    /// Operate on the 'a'-th arg
    virtual result_t operator()(alignment&,Parameters&,int a)=0;

    MoveArg(const string& s):Move(s) { }

    virtual ~MoveArg() {}
  };


  /// Apply this move to Each arg

  /// Apply this move to Each arg, choosing for each arg from
  /// among the moves that apply to that arg.
  class MoveEach: public MoveArg, protected MoveGroupBase {

    /// The n-th arg of this move, is the subarg[m][n]-th arg of move m
    vector< vector<int> > subarg;

    /// Get the sum of the weights for the i-th arg
    double sum(int i) const;

    /// Choose an submove to apply to arg i
    int choose(int i) const;

    /// Is the arg-th arg is available for the m-th move?
    bool submove_has_arg(int m,int arg) const {return subarg[m][arg] != -1;}
  public:
    MoveEach* clone() const {return new MoveEach(*this);}

    void add(double l,const MoveArg& m,bool=true);

    void enable(const string&);
    void disable(const string&);

    result_t operator()(alignment&,Parameters&,int);
    
    void show_enabled(int depth=0) const;
    void print_move_stats(int depth=0) const;

    MoveEach(const string& s):MoveArg(s) {}

    virtual ~MoveEach() {}
  };

  /// A single move with an integer argument, and the arguments it takes
  class MoveArgSingle: public MoveArg {
    atomic_move_arg m;
  public:
    MoveArgSingle* clone() const {return new MoveArgSingle(*this);}

    result_t operator()(alignment&,Parameters&,int);

    MoveArgSingle(const string& s,atomic_move_arg m1,const vector<int>& a)
      :MoveArg(s),m(m1) 
    {args=a;}

    ~MoveArgSingle() {}
  };



  /// A Sampler: based on a collection of moves to run every iteration
  class Sampler: public MoveAll {

  public:
    /// Run the sampler for 'max' iterations
    void go(alignment& A, Parameters& P, int subsample, int max);

    Sampler(const string& name)
      :MoveAll(name) {};
  };

}

#endif
