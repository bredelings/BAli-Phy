/*
   Copyright (C) 2004-2010 Benjamin Redelings

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

/* TODO
   0. Can we have a triplet model that isn't derived from M0*Neutral?

   1. The WithINV models currently have INV frequencies = variant frequencies.
    a) If we had *2* submodels, then INV and the other models could have
       DIFFERENT frequencies.

   2. If we want to do M0 + M7 + AAINV, then 
      a) how do we make the AAINV model share the same alphabet model?
      b) could we make it share the same TripletFrequencies but
         use a different CodonFrequencies, and so have different AA 
         frequencies?
      c) can we use '+' to represent mixtures, instead of stacking?
         M0~[w=0 + w~Beta( ) + w=1]
         WAG~[rate=0 + rate~Gamma( )]  vs WAG~[INV + gamma] + WAG~[gamma+INV]
      d) It looks like we need to have a special class to specify a PRIOR on the mixture
         probabilities, though.
      e) How about M0~[w=0] instead of Conserved?
 
   3. Compare the marginal probability of CodonFrequencyModels on bglobin
      (fixed A) at different settings:
      a) f = 0.5 / nuX = 1/n (g=1) / c=0 (b_X=1/n, h=0) -> HKY (should match)
      b) fX=1/n (f=1) / no constraint / c=0 (b_X=1/n, h=0) -> M0 (should match)
      c) fX=1/n (f=1) / nX = 1/n (g=1) / c=1 -> AA flat model
      d) fX=1/n (f=1) / no constraint /  c=1 -> AA flat model w/ codon bias
      e) no constraint / no constraint / c=1 -> AA flat model w/ codon bias + nuc. freqs.
      f) no constraint / no constraint / no constraint

   4. What if we want to do {M0 + M7 + AAINV + Neutral} ?
      We should perhaps consider mixtures of exchange models.
      Would this handle gamma?

   5. We could handle exchange models and frequencies separately:
      {WAG ~ [gamma+INV]}*{multifreq}

   6. Could we do multi-codonfrequency models? (e.g. decide to share the nuc. freq.
      model, but have different aa freqs)

   7. Is there a more natural separation of this file?
      NOTE: lots of this use {get,set}_varray( ), etc.
      Maybe just separate the headers.

   8. In the future, specifying priors and proposals could hopefully be separated
      from models [or, at least, model structure]

   9. In order to construct models -- like the branch-site model -- with different
      Q matrices on different branches, we need to be able to generate transition
      matrix collections of the form transition_P(m,b).  

*/

#ifndef SMODEL_H
#define SMODEL_H

#include "smodel/objects.H"
#include <valarray>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/numeric/ublas/symmetric.hpp>
#include "mytypes.H"
#include "alphabet.H"
#include "rates.H"
#include "model.H"
#include "eigenvalue.H"
#include "cached_value.H"

namespace substitution {

  using std::string;
  using std::valarray;

  //--------------------- class AlphabetModel ----------------------//

  /// A generic model of the process of substitution
  class Model: virtual public ::Model
  {
  public:
    /// Make a copy of this object
    virtual Model* clone() const =0;

    virtual ~Model() {}
  };


  //----------------------- Nested Models ------------------------///

  // This models a substitution process in which evolution is independent between
  // non-overlapping intervals.  We should have that transition_p(t/2)^2 = transition_p(t)
  // This model must also be reversible

  class ReversibleModel: virtual public Model 
  {
  public:
    /// Make a copy of this object
    virtual ReversibleModel* clone() const =0;

    ReversibleModel() {}
  };

  /// An AdditiveModel depends only on t2-t1

  /// In additive models P(X[t+tau]=b|X[t]=a) = P(X[tau]=b|X[0]=a)
  /// Also E f(X,(a,b)) = E f(X,(0,b-a))

  class AdditiveModel: virtual public Model 
  {
  public:
    virtual AdditiveModel* clone() const=0;

    AdditiveModel() {}
  };

  class ReversibleAdditiveModel: virtual public ReversibleModel, virtual public AdditiveModel 
  {
  public:
    virtual ReversibleAdditiveModel* clone() const=0;
  };

  // With an Independant Model, you can chain transition matrices.
  // Q_t(a,b) = sum_c Q[0,tau](a,c) x Q[tau,t](c,b)

  // In an independent additive
  class IndependantAdditiveModel: virtual public AdditiveModel 
  {
  public:
    virtual IndependantAdditiveModel* clone() const=0;

    IndependantAdditiveModel() {}
  };

  class Independant_Additive_Reversible_Model: public ReversibleAdditiveModel, 
					       virtual public IndependantAdditiveModel
  {
  public:
    virtual Independant_Additive_Reversible_Model* clone() const=0;
  };


  class ExchangeModel: virtual public ::Model
  {
  public:
    
    ExchangeModel* clone() const =0;
    
    virtual ~ExchangeModel() {};
  };

  inline boost::shared_ptr<const alphabet> get_alphabet(const ::Model& M)
  {
    return dynamic_cast<const SModelObject&>(*M.result()).get_alphabet();
  }
    
  class AlphabetExchangeModel: public ExchangeModel
  {
  public:
    AlphabetExchangeModel* clone() const =0;
    
    virtual ~AlphabetExchangeModel() {};
  };

  
  //--------------------- EQU Substitution Model ------------------------/

  /// A generic model for any alphabet - the exchangeability matrix is loaded from a file
  class Empirical: public AlphabetExchangeModel
  {
    /// Specificy this model based on the matrix that was loaded
    string name_;

  public:
    /// Load the exhangeability matrix from file 'filename'
    void load_file(std::istream&);

    /// Load the exhangeability matrix from file 'filename'
    void load_file(const string& filename);

    virtual Empirical* clone() const {return new Empirical(*this);}

    string name() const {return name_;}

    boost::shared_ptr<const Object> result() const;

    Empirical(const alphabet& a); 

    Empirical(const alphabet& a, const string& n); 

    ~Empirical() {}
  };

  class PAM: public Empirical
  {
  public:
    virtual PAM* clone() const {return new PAM(*this);}

    PAM();
  };

  class JTT: public Empirical
  {
  public:
    virtual JTT* clone() const {return new JTT(*this);}

    JTT();
  };

  class WAG: public Empirical
  {
  public:
    virtual WAG* clone() const {return new WAG(*this);}

    WAG();
  };

  class LG: public Empirical
  {
  public:
    virtual LG* clone() const {return new LG(*this);}

    LG();
  };

  class NucleotideExchangeModel: public AlphabetExchangeModel
  {
  public:
    NucleotideExchangeModel* clone() const=0;
  };

  class TripletExchangeModel: public AlphabetExchangeModel
  {
  public:
    TripletExchangeModel* clone() const=0;

    TripletExchangeModel(const Triplets& T);

    ~TripletExchangeModel() {}
  };


  //------------------------ Reversible frequency model --------------------//

  class ReversibleFrequencyModel: virtual public ::Model
  {
  public:
    ReversibleFrequencyModel* clone() const =0;

    ReversibleFrequencyModel(const alphabet& a);

    virtual ~ReversibleFrequencyModel() {}
  };



  class SimpleFrequencyModel: public ReversibleFrequencyModel
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}
    void update() { }
  public:

    boost::shared_ptr<const Object> result() const;

    SimpleFrequencyModel* clone() const {return new SimpleFrequencyModel(*this);}

    efloat_t prior() const;

    string name() const;

    explicit SimpleFrequencyModel(const alphabet& a);

    explicit SimpleFrequencyModel(const alphabet& a,const valarray<double>&);
  };

  class UniformFrequencyModel: public ReversibleFrequencyModel
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    
    boost::shared_ptr<const Object> result() const;

    UniformFrequencyModel* clone() const {return new UniformFrequencyModel(*this);}

    string name() const;

    explicit UniformFrequencyModel(const alphabet& a);

    explicit UniformFrequencyModel(const alphabet& a,const valarray<double>&);
  };

  class TripletFrequencyModel: public ReversibleFrequencyModel
  {
  public:
    TripletFrequencyModel* clone() const =0;

    explicit TripletFrequencyModel(const Triplets& T);
  };

  class IndependentNucleotideFrequencyModel: public TripletFrequencyModel,
					     public SuperModelOver<SimpleFrequencyModel>
  {
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    boost::shared_ptr<const Object> result() const;

    IndependentNucleotideFrequencyModel* clone() const {return new IndependentNucleotideFrequencyModel(*this);}

    string name() const;

    IndependentNucleotideFrequencyModel(const Triplets& T);
  };


  class TripletsFrequencyModel: public TripletFrequencyModel,
				public SuperModelOver<SimpleFrequencyModel>
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    boost::shared_ptr<const Object> result() const;

    TripletsFrequencyModel* clone() const {return new TripletsFrequencyModel(*this);}

    efloat_t super_prior() const;

    string name() const;

    explicit TripletsFrequencyModel(const Triplets& T);
  };

  class CodonFrequencyModel: public ReversibleFrequencyModel
  {
  public:
    CodonFrequencyModel* clone() const =0;

    explicit CodonFrequencyModel(const Codons& C);
  };

  class AACodonFrequencyModel: public CodonFrequencyModel,
			       public SuperModelOver<SimpleFrequencyModel>
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}
  public:
    boost::shared_ptr<const Object> result() const;

    AACodonFrequencyModel* clone() const {return new AACodonFrequencyModel(*this);}

    string name() const;

    explicit AACodonFrequencyModel(const Codons& C);
  };

  class CodonsFrequencyModel: public CodonFrequencyModel,
			      public SuperModelOver<TripletsFrequencyModel>
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    boost::shared_ptr<const Object> result() const;

    CodonsFrequencyModel* clone() const {return new CodonsFrequencyModel(*this);}

    efloat_t super_prior() const;

    string name() const;

    explicit CodonsFrequencyModel(const Codons& C);
  };

  
  class CodonsFrequencyModel2: public CodonFrequencyModel,
			      public SuperModelOver<TripletsFrequencyModel>
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    boost::shared_ptr<const Object> result() const;

    CodonsFrequencyModel2* clone() const {return new CodonsFrequencyModel2(*this);}

    efloat_t super_prior() const;

    string name() const;

    explicit CodonsFrequencyModel2(const Codons& C);
  };

  

  //------------------------- Reversible Substitution Model -----------------------//

  // Store S2 = pi^0.5 * S * pi^0.5
  // Store EigenValues(S2) -> D,O
  // Is this worth it?  Try it later...

  // exp(Qt) ->
  //   1. get Dt = D*exp(t)
  //   2. get O*Dt*O
  //   3. return DN*(O*Dt*O)*DP

  /// A Markov substitution model which is reversible
  class ReversibleMarkovModel: public Independant_Additive_Reversible_Model 
  {
  public:

    /// Make a copy of this object
    virtual ReversibleMarkovModel* clone() const =0;

    ~ReversibleMarkovModel() {}
  };

  /// A Felsenstein-81 model, in which Q_ij = a*pi[j]
  class F81_Model: public ReversibleMarkovModel
  {
    void recalc(const std::vector<int>&) {std::abort();}
  public:
    /// Make a copy of this object
    virtual F81_Model* clone() const {return new F81_Model(*this);}

    virtual efloat_t prior() const;

    string name() const;

    boost::shared_ptr<const Object> result() const;

    F81_Model(const alphabet& a);
    F81_Model(const alphabet& a,const valarray<double>&);
    ~F81_Model() {}
  };


  //--------------------------- Implement the underlying alphabet -------------------------//


  template <class T>
  class ReversibleWrapperOver: virtual public ReversibleModel,
			       public SuperModelOver< ::Model>
  {
  public:
    T& SubModel() {return SubModelAs<T>(0); }

    const T& SubModel() const { return SubModelAs<T>(0); }

    virtual ReversibleWrapperOver* clone() const=0;

    ReversibleWrapperOver(const T& M) {
      insert_submodel("BASE",M);
    }
  };

  //----------------------- MultiModel ---------------------------*/

  class MultiModel: public ReversibleAdditiveModel
  {

  public:
    virtual MultiModel* clone() const=0;

    ~MultiModel() {}
  };

  /// A substitution model based on running identical copies of an input model with different frequencies.
  class CAT_FixedFrequencyModel: public MultiModel
  {
  protected:
    /// Fraction of the sites with each parameter value, according to model estimate
    std::vector<double> prior_fraction;

    string name_;

    void recalc(const std::vector<int>&) {std::abort();}

    void update() { }
  public:
    const alphabet& Alphabet() const;

    /// Load the site frequencies from \a file
    void load_file(std::istream& file);

    /// Load the site frequencies from \a filename
    void load_file(const string& filename);

    // no parameters: super_prior() == 1

    CAT_FixedFrequencyModel* clone() const {return new CAT_FixedFrequencyModel(*this);}

    efloat_t prior() const;

    string name() const {return name_;}

    boost::shared_ptr<const Object> result() const;

    /// Create a CAT_FixedFrequencyModel with alphabet \a a.
    CAT_FixedFrequencyModel(const alphabet& a);

    /// Create a CAT_FixedFrequencyModel with alphabet \a a and name \a n.
    CAT_FixedFrequencyModel(const alphabet& a, const string& n);

    ~CAT_FixedFrequencyModel() {}
  };


  class C10_CAT_FixedFrequencyModel: public CAT_FixedFrequencyModel
  {
  public:
    C10_CAT_FixedFrequencyModel* clone() const {return new C10_CAT_FixedFrequencyModel(*this);}

    C10_CAT_FixedFrequencyModel();
  };

  class C20_CAT_FixedFrequencyModel: public CAT_FixedFrequencyModel
  {
  public:
    C20_CAT_FixedFrequencyModel* clone() const {return new C20_CAT_FixedFrequencyModel(*this);}

    C20_CAT_FixedFrequencyModel();
  };
}

#endif


