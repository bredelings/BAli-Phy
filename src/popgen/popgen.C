#include <string>
#include <vector>
#include <map>
#include "popgen.H"
#include "computation/operation.H"
#include "computation/computation.H"
#include "computation/prelude.H"
#include "probability/distribution-operations.H"
#include "io.H"

using std::string;
using std::vector;
using std::map;

struct Read_PHASE_File: public Operation
{
  Read_PHASE_File* clone() const {return new Read_PHASE_File(*this);}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    
    if (typeid(*this) != typeid(O)) return false;
    
    return true;
  }
  
  closure operator()(OperationArgs& Args) const;
  
  std::string name() const {return "Read_PHASE_File";}
  
  Read_PHASE_File():Operation(1) { }
};

template <typename T> 
T get_line_of(std::istream& i)
{
  string line;
  portable_getline(i, line);
  return convertTo<T>(line);
}

closure Read_PHASE_File::operator()(OperationArgs& Args) const
{
  const string& filename = *Args.evaluate_as<String>(0);

  checked_ifstream phase_file(filename,"PHASE Input file");

  // Line 1
  int n_individuals = get_line_of<int>(phase_file);

  // Line 2
  int n_loci = get_line_of<int>(phase_file);

  // Line 3
  string line;
  portable_getline(phase_file, line);

  if (line.size() != n_loci)  throw myexception()<<"Loci description has "<<line.size()<<" loci, but header says there are "<<n_loci<<".";

  for(int i=0;i<line.size();i++)
    if (line[i] != 'M')
      throw myexception()<<"Locus "<<i+1<<" is not a microsatellite locus!";

  // Lines 4- for each individual.
  Vector<Vector<int>> result;
  for(int i=0;i<n_individuals;i++)
  {
    portable_getline(phase_file, line);
    vector<string> words = split(line, '\t');
    words.erase(words.begin());
    Vector<int> loci;
    for(const auto& word: words)
      loci.t.push_back(convertTo<int>(word));
    result.t.push_back(loci);
  }

  return result;
}

struct Remove_2nd_Allele: public Operation
{
  Remove_2nd_Allele* clone() const {return new Remove_2nd_Allele(*this);}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    
    if (typeid(*this) != typeid(O)) return false;
    
    return true;
  }
  
  closure operator()(OperationArgs& Args) const;
  
  std::string name() const {return "Remove_2nd_Allele";}
  
  Remove_2nd_Allele():Operation(1) { }
};

closure Remove_2nd_Allele::operator()(OperationArgs& Args) const
{
  object_ptr<const Vector<Vector<int>>> alleles = Args.evaluate_as<Vector<Vector<int>>>(0);

  Vector<Vector<int>> alleles2 = *alleles;

  for(auto& v:alleles2.t)
    for(int i=v.t.size()-1;i>=0;i-=2)
      v.t.erase(v.t.begin() + i);

  return alleles2;
}

struct Allele_Frequency_Spectrum: public Operation
{
  Allele_Frequency_Spectrum* clone() const {return new Allele_Frequency_Spectrum(*this);}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    
    if (typeid(*this) != typeid(O)) return false;
    
    return true;
  }
  
  closure operator()(OperationArgs& Args) const;
  
  std::string name() const {return "Allele_Frequency_Spectrum";}
  
  Allele_Frequency_Spectrum():Operation(1) { }
};

closure Allele_Frequency_Spectrum::operator()(OperationArgs& Args) const
{
  const vector<Vector<int>>& alleles = *Args.evaluate_as<Vector<Vector<int>>>(0);

  int n_individuals = alleles.size();
  assert(n_individuals > 0);

  int n_loci = alleles[0].t.size();

  Vector<Vector<int>> afs_;
  vector<Vector<int>>& afs = afs_.t;

  for(int l=0;l<n_loci;l++)
  {
    // 1. Count the alleles of each type
    map<int,int> allele_counts;
    for(int i=0;i<n_individuals;i++)
      allele_counts[alleles[i].t[l]]++;

    // 2. Determine how many alleles with each count there are.
    vector<int> spectrum(n_individuals,0);
    for(const auto& allele_and_count: allele_counts)
      spectrum[allele_and_count.second-1]++;

    afs.push_back(spectrum);
  }

  return afs_;
}

log_double_t factorial(int n)
{
  log_double_t f = 1;
  for(int i=2;i<=n;i++)
    f *= i;
  return f;
}

log_double_t ewens_sampling_probability(double theta, const vector<int>& a)
{
  assert(theta >= 0.0);

  const int n = a.size();

  log_double_t Pr = 1;

  // 2. Compute probability
  for(int i=1;i<=n;i++)
  {
    int a_i = a[i-1];
    Pr *= (double(i)/(theta+i-1));
    if (a_i > 0) 
    {
      log_double_t x = theta/i;
      Pr *= pow(x,a_i)/factorial(a_i);
    }
  }

  return Pr;
}

struct Ewens_Sampling_Group_Probability: public Operation
{
  Ewens_Sampling_Group_Probability* clone() const {return new Ewens_Sampling_Group_Probability(*this);}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    
    if (typeid(*this) != typeid(O)) return false;
    
    return true;
  }
  
  closure operator()(OperationArgs& Args) const;
  
  std::string name() const {return "Ewens_Sampling_Group_Probability";}
  
  Ewens_Sampling_Group_Probability():Operation(2) { }
};

closure Ewens_Sampling_Group_Probability::operator()(OperationArgs& Args) const
{
  const double theta = *Args.evaluate_as<Double>(0);
  const vector<Vector<int>>& afs = *Args.evaluate_as<Vector<Vector<int>>>(1);

  log_double_t Pr = 1;
  for(const auto& a: afs)
    Pr *= ewens_sampling_probability(theta,a.t);

  return Log_Double(Pr);
}

struct Ewens_Sampling_Probability: public Operation
{
  Ewens_Sampling_Probability* clone() const {return new Ewens_Sampling_Probability(*this);}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    
    if (typeid(*this) != typeid(O)) return false;
    
    return true;
  }
  
  closure operator()(OperationArgs& Args) const;
  
  std::string name() const {return "Ewens_Sampling_Probability";}
  
  Ewens_Sampling_Probability():Operation(2) { }
};

closure Ewens_Sampling_Probability::operator()(OperationArgs& Args) const
{
  const double theta = *Args.evaluate_as<Double>(0);
  const vector<int>& afs = *Args.evaluate_as<Vector<int>>(1);

  log_double_t Pr = ewens_sampling_probability(theta,afs);

  return Log_Double(Pr);
}

struct Ewens_Sampling_Mixture_Probability: public Operation
{
  Ewens_Sampling_Mixture_Probability* clone() const {return new Ewens_Sampling_Mixture_Probability(*this);}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    
    if (typeid(*this) != typeid(O)) return false;
    
    return true;
  }
  
  closure operator()(OperationArgs& Args) const;
  
  std::string name() const {return "builtin_Ewens_Sampling_Mixture_Probability";}
  
  Ewens_Sampling_Mixture_Probability():Operation(3) { }
};

closure Ewens_Sampling_Mixture_Probability::operator()(OperationArgs& Args) const
{
  const vector<double>& thetas = *Args.evaluate_as<Vector<double>>(0);
  const vector<double>& ps = *Args.evaluate_as<Vector<double>>(1);
  const vector<Vector<int>>& afs = *Args.evaluate_as<Vector<Vector<int>>>(2);

#ifndef NDEBUG
  for(int i=0;i<thetas.size();i++)
  {
    assert(thetas[i] >= 0.0);
    assert(ps[i] >= 0.0);
    assert(ps[i] <= 1.0);
  }
#endif

  log_double_t Pr = 1;
  for(const auto& a: afs)
  {
    double pr = 0;
    for(int i=0;i<thetas.size();i++)
      pr += ps[i] * ewens_sampling_probability(thetas[i],a.t);

    Pr *= pr;
  }

  return Log_Double(Pr);
}

Program PopGen_Functions()
{
  Program P("PopGen");
  P.import_module(get_Prelude(), false);
  P.import_module(Distribution_Functions(), false);
  P.def_function("readPhaseFile", lambda_expression(Read_PHASE_File()));
  P.def_function("remove2ndAllele", lambda_expression(Remove_2nd_Allele()));
  P.def_function("alleleFrequencySpectrum", lambda_expression(Allele_Frequency_Spectrum()));
  P.def_function("ewensSamplingProbability", lambda_expression(Ewens_Sampling_Probability()));
  P.def_function("ewensSamplingGroupProbability", lambda_expression(Ewens_Sampling_Group_Probability()));
  P.def_function("builtinEwensSamplingMixtureProbability", lambda_expression(Ewens_Sampling_Mixture_Probability()));

  P += "{ewensSamplingMixtureProbability (thetas,ps) x = builtinEwensSamplingMixtureProbability (listToVectorDouble thetas) (listToVectorDouble ps) x}";

  P += "{afs args = (ProbDensity \"afs\" ewensSamplingProbability (error \"afs has no quantile\") () (),args)}";

  P += "{afsGroup args = (ProbDensity \"afsGroup\" ewensSamplingGroupProbability (error \"afs has no quantile\") () (),args)}";

  P += "{afsMixture args = (ProbDensity \"afsMixture\" ewensSamplingMixtureProbability (error \"afs has no quantile\") () (),args)}";

  return P;
}
