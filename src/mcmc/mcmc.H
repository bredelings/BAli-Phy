/*
   Copyright (C) 2004-2011 Benjamin Redelings

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

///
/// \file   mcmc.H
/// \brief  Provides classes for constructing MCMC samplers.
///
/// This file provides classes for constructing MCMC samplers.  The
/// class Sampler is used to run the main loop of the sampler for
/// bali-phy.
///
/// \author Benjamin Redelings
/// 

#ifndef MCMC_H
#define MCMC_H

#include <iostream>
#include <vector>
#include <valarray>
#include <string>
#include <map>
#include "models/parameters.H"
#include "rng.H"
#include "proposals.H"
#include "bounds.H"
#include "logger.H"

// how to have different models, with different moves
// and possibly moves between models?

struct slice_function;

namespace MCMC {

  //---------------------- Move Stats ---------------------//
  /// \brief Stores counts, totals, and averages for an MCMC transition kernel
  /// 
  /// The averages for the i-th statistic are totals[i]/counts[i]. For 
  /// example, the success rate for an MH transition kernel would have a
  /// count (the number of moves run) and a total (the number of successes)
  /// resulting in an average (the fraction of successes).
  struct Result 
  {
    /// The number of trials for each statistic
    std::valarray<int> counts;

    /// The sum of values over the trials
    std::valarray<double> totals;

    /// The number of statistics we are calculating
    int size() const {return counts.size();}

    /// Increment counts and totals by the counts and totals given.
    void inc(const Result&);

    Result() {}
    Result(bool);
    Result(int,int=1);
  };

  class MoveStats: public std::map<std::string,Result>
  {
  public:
    void inc(const std::string&, const Result&);
  };

  //---------------------- Simple Move  ---------------------//
  typedef void (*atomic_move)(owned_ptr<Probability_Model>&,MoveStats&);
  typedef void (*atomic_move_arg)(owned_ptr<Probability_Model>&,MoveStats&,int);

  //---------------- Move's w/ sub-moves --------------------//
  class Move: public Object
  {
    bool enabled_;

  public:
    std::string name;

    std::vector<std::string> attributes;

    virtual std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const = 0;

    double iterations;

    /// Make a copy of this object
    virtual Move* clone() const =0;

    /// Is this move enabled?
    bool enabled() const {return enabled_;}

    /// Enable this move
    void enable() {enabled_=true;}

    /// Disable this move
    void disable() {enabled_=false;}
    
    /// Start learning
    virtual void start_learning(int) {}

    /// Stop learning
    virtual void stop_learning(int) {}

    /// Enable this move or any submove with name or attribute 's'
    virtual void enable(const std::string& s);

    /// Disable this move or any submove with name or attribute 's'
    virtual void disable(const std::string& s);

    /// Set up for an iteration of length l, return number of subiterations
    virtual int reset(double l);

    /// Do a complete iteration (a top-level routine)
    virtual void iterate(owned_ptr<Probability_Model>&,MoveStats&) =0;

    /// Do the i-th iteration for this round (not a top-level routine)
    virtual void iterate(owned_ptr<Probability_Model>&,MoveStats&,int i) =0;

    /// Show enabled-ness for this move and submoves
    virtual void show_enabled(std::ostream&,int depth=0) const;

    /// construct a new move called 's'
    Move(const std::string& s);
    Move(const std::string& s, const std::string& v);

    virtual ~Move() {}
  };

  // FIXME? We could make this inherit from virtual public Move...
  //    but that seems to introduce problems...
  // We could also move the code to this class, and call it from the classes
  //    that use it.

  /// A collection of moves
  class MoveGroupBase 
  {
  protected:
    /// This list of moves
    std::vector<owned_ptr<Move> > moves;

    /// This weight of each move
    std::vector<double> lambda;

  public:

    int nmoves() const {return moves.size();}
    void add(double,const Move& m,bool=true);

    MoveGroupBase() {}
  };

  /// A Move which runs parts of a number of submoves each round
  class MoveGroup: public Move,public MoveGroupBase 
  {
  protected:

    /// An ordered list of submoves to run this round
    std::vector<int> order;

    /// suborder[i] is the n-th time we've run order[i]
    std::vector<int> suborder;
    
    double sum() const;

    /// Setup 'order' and 'suborder' for this round
    virtual void getorder(double l)=0;
  public:

    MoveGroup* clone() const =0;

    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const;

    void enable(const std::string&);
    void disable(const std::string&);

    /// Start learning
    void start_learning(int);
    /// Stop learning
    void stop_learning(int);

    int reset(double);
    void iterate(owned_ptr<Probability_Model>&,MoveStats&);
    void iterate(owned_ptr<Probability_Model>&,MoveStats&,int);

    void show_enabled(std::ostream&,int depth=0) const;

    MoveGroup(const std::string& s):Move(s) {}
    MoveGroup(const std::string& s, const std::string& v):Move(s,v) {}

    virtual ~MoveGroup() {}
  };

  /// A Move which runs all submoves each round
  class MoveAll: public MoveGroup {
    void getorder(double l);
  public:
    MoveAll* clone() const {return new MoveAll(*this);}

    MoveAll(const std::string& s):MoveGroup(s) {}
    MoveAll(const std::string& s, const std::string& v):MoveGroup(s,v) {}
    virtual ~MoveAll() {}
  };

  /// A Move which runs one of its submoves each round
  class MoveOne: public MoveGroup 
  {
    int choose() const;
    void getorder(double l);
  public:
    MoveOne* clone() const {return new MoveOne(*this);}

    MoveOne(const std::string& s):MoveGroup(s) {}
    MoveOne(const std::string& s, const std::string& v):MoveGroup(s,v) {}
    virtual ~MoveOne() {}
  };

  /// A Move which runs a specific move each round
  class SingleMove: public Move {
    atomic_move m;
    std::set<int> affected_parameters;
  public:
    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const {return affected_parameters;}

    SingleMove* clone() const {return new SingleMove(*this);}

    int reset(double lambda);

    void iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) { iterate(P,Stats,0); }

    void iterate(owned_ptr<Probability_Model>& P,MoveStats&,int);

    SingleMove(atomic_move m1,const std::string& s, const std::set<int>& a)
      :Move(s),m(m1),affected_parameters(a) {}
    SingleMove(atomic_move m1,const std::string& s, const std::set<int>& a, const std::string& v)
      :Move(s,v),m(m1),affected_parameters(a) {}
  };


  /// A Move which runs a specific moves each round
  class MH_Move: public Move {
    owned_ptr<Proposal> proposal;
    std::set<int> affected_parameters;
  public:
    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>& P) const {
      return proposal->get_affected_parameters(P);
    }

    MH_Move* clone() const {return new MH_Move(*this);}

    int reset(double lambda);

    void iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) { iterate(P,Stats,0); }

    // FIXME, can I get rid of this for all groups that aren't using it?
    void iterate(owned_ptr<Probability_Model>& P,MoveStats&,int);

    MH_Move(const Proposal& P,const std::string& s)
      :Move(s),proposal(P) {}
    MH_Move(const Proposal& P,const std::string& s, const std::string& v)
      :Move(s,v),proposal(P) {}
  };

  // Improve: make W into a FUNCTION to determine the initial width
  class Slice_Move: public Move
  {
  protected:
    double W;

    double (*transform)(double);
    double (*inverse)(double);

    int n_learning_iterations;
    int n_tries;
    double total_movement;

  public:
    Slice_Move* clone() const=0;

    double sample(Probability_Model& P, slice_function& f, double);

    void start_learning(int);

    void stop_learning(int);

    Slice_Move(const std::string& s);

    Slice_Move(const std::string& s, const std::string& v);

    Slice_Move(const std::string& s, double W_);

    Slice_Move(const std::string& s, const std::string& v, double W_);

    Slice_Move(const std::string& s, const std::string& v,
	       double W_,
	       double(*f1)(double),
	       double(*f2)(double));
  };

  class Parameter_Slice_Move: public Slice_Move
  {
    int index;

  public:
    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const {return {index};}

    Parameter_Slice_Move* clone() const {return new Parameter_Slice_Move(*this);}

    void iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) { iterate(P,Stats,0); }

    void iterate(owned_ptr<Probability_Model>& P,MoveStats&,int);

    Parameter_Slice_Move(const std::string& s,int i, double W_);

    Parameter_Slice_Move(const std::string& s, const std::string& v,int i,
			 double W_);

    Parameter_Slice_Move(const std::string& s,int i,
			 double (*W_)(const Probability_Model&));

    Parameter_Slice_Move(const std::string& s, const std::string& v,int i,
			 double (*W_)(const Probability_Model&));

    Parameter_Slice_Move(const std::string& s,int i,
			 double W_,
			 double(*f1)(double),
			 double(*f2)(double));

    Parameter_Slice_Move(const std::string& s, const std::string& v,int i,
			 double W_,
			 double(*f1)(double),
			 double(*f2)(double));
  };

  class Modifiable_Slice_Move: public Slice_Move
  {
    int m_index;
    int p_index;
    Bounds<double> bounds;

  public:
    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const {return {};}
    
    Modifiable_Slice_Move* clone() const {return new Modifiable_Slice_Move(*this);}
    
    void iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) { iterate(P,Stats,0); }
    
    void iterate(owned_ptr<Probability_Model>& P,MoveStats&,int);
    
    Modifiable_Slice_Move(const std::string& s,int m, int p, const Bounds<double>& b, double W_);
    
    Modifiable_Slice_Move(const std::string& s, const std::string& v,int m, int p,
			  const Bounds<double>& b, double W_);
    
    Modifiable_Slice_Move(const std::string& s,int m, int p,
			  const Bounds<double>& b, double (*W_)(const Probability_Model&));
    
    Modifiable_Slice_Move(const std::string& s, const std::string& v,int m, int p,
			  const Bounds<double>& b, double (*W_)(const Probability_Model&));
    
    Modifiable_Slice_Move(const std::string& s,int m, int p,
			  const Bounds<double>& b, double W_,
			  double(*f1)(double),
			  double(*f2)(double));
    
    Modifiable_Slice_Move(const std::string& s, const std::string& v,int m, int p,
			  const Bounds<double>& b, double W_,
			  double(*f1)(double),
			  double(*f2)(double));
  };

  class Dirichlet_Slice_Move: public Slice_Move
  {
    std::vector<int> indices;

    int n;

  public:
    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const;

    Dirichlet_Slice_Move* clone() const {return new Dirichlet_Slice_Move(*this);}

    void iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) { iterate(P,Stats,0); }

    void iterate(owned_ptr<Probability_Model>& P,MoveStats&,int);

    Dirichlet_Slice_Move(const std::string&, const std::vector<int>&, int);
  };

  class Dirichlet_Modifiable_Slice_Move: public Slice_Move
  {
    std::vector<int> indices;

    int n;

  public:
    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const;

    Dirichlet_Modifiable_Slice_Move* clone() const {return new Dirichlet_Modifiable_Slice_Move(*this);}

    void iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) { iterate(P,Stats,0); }

    void iterate(owned_ptr<Probability_Model>& P,MoveStats&,int);

    Dirichlet_Modifiable_Slice_Move(const std::string&, const std::vector<int>&, int);
  };

  class Scale_Means_Only_Slice_Move: public Slice_Move
  {
  public:
    Scale_Means_Only_Slice_Move* clone() const {return new Scale_Means_Only_Slice_Move(*this);}
 
    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const;

    void iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) { iterate(P,Stats,0); }

    void iterate(owned_ptr<Probability_Model>& P,MoveStats&,int);

    Scale_Means_Only_Slice_Move(const std::string&, double);
  };

  /// A move which takes an integer argument from a supplied list
  class MoveArg: public Move {
    /// The ordered list of args to operate on this round
    std::vector<int> order;

  public:
    MoveArg* clone() const=0;

    std::set<int> get_affected_parameters(const owned_ptr<Probability_Model>&) const {return {};}

    /// A list of arguments to be passed to submoves
    std::vector<int> args;

    int reset(double);
    void iterate(owned_ptr<Probability_Model>&,MoveStats&);
    void iterate(owned_ptr<Probability_Model>&,MoveStats&,int);

    /// Operate on the 'a'-th arg
    virtual void operator()(owned_ptr<Probability_Model>&,MoveStats&,int a)=0;

    MoveArg(const std::string& s):Move(s) { }
    MoveArg(const std::string& s, const std::string& v):Move(s,v) { }

    virtual ~MoveArg() {}
  };


  /// Apply this move to Each arg

  /// Apply this move to Each arg, choosing for each arg from
  /// among the moves that apply to that arg.
  class MoveEach: public MoveArg, protected MoveGroupBase {

    /// The n-th arg of this move, is the subarg[m][n]-th arg of move m
    std::vector< std::vector<int> > subarg;

    /// Get the sum of the weights for the i-th arg
    double sum(int i) const;

    /// Choose an submove to apply to arg i
    int choose(int i) const;

    /// Is the arg-th arg is available for the m-th move?
    bool submove_has_arg(int m,int arg) const {return subarg[m][arg] != -1;}
  public:
    MoveEach* clone() const {return new MoveEach(*this);}

    void add(double l,const MoveArg& m,bool=true);

    void enable(const std::string&);
    void disable(const std::string&);

    void operator()(owned_ptr<Probability_Model>&,MoveStats&,int);
    
    void show_enabled(std::ostream&,int depth=0) const;

    MoveEach(const std::string& s):MoveArg(s) {}
    MoveEach(const std::string& s,const std::string& v):MoveArg(s,v) {}

    virtual ~MoveEach() {}
  };

  /// A single move with an integer argument, and the arguments it takes
  class MoveArgSingle: public MoveArg {
    atomic_move_arg m;
  public:
    MoveArgSingle* clone() const {return new MoveArgSingle(*this);}

    void operator()(owned_ptr<Probability_Model>&,MoveStats&,int);

    MoveArgSingle(const std::string& s,atomic_move_arg m1,const std::vector<int>& a)
      :MoveArg(s),m(m1) 
    {args=a;}

    MoveArgSingle(const std::string& s,const std::string& v,atomic_move_arg m1,const std::vector<int>& a)
      :MoveArg(s,v),m(m1) 
    {args=a;}

    ~MoveArgSingle() {}
  };

  /// A Sampler: based on a collection of moves to run every iteration
  class Sampler: public MoveAll, public MoveStats 
  {
    std::vector<owned_ptr<Logger> > loggers;

  public:
    /// Run the sampler for 'max' iterations
    void go(owned_ptr<Probability_Model>& P, int subsample, int max, std::ostream&);

    void check_moves(const owned_ptr<Probability_Model>& P) const;

    int n_loggers() const {return loggers.size();}

    void add_logger(const owned_ptr<Logger>&);

    Sampler(const std::string& s)
      :MoveAll(s) {}
  };

}

std::ostream& operator <<(std::ostream& o, const MCMC::MoveStats& Stats);

namespace MCMC
{
  using ::operator<<;
}
#endif
