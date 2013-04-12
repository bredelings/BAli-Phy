#include <string>
#include <vector>
#include <map>
#include "computation/computation.H"
#include "io.H"

using std::string;
using std::vector;
using std::map;

template <typename T> 
T get_line_of(std::istream& i)
{
  string line;
  portable_getline(i, line);
  return convertTo<T>(line);
}

extern "C" closure builtin_function_read_phase_file(OperationArgs& Args)
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
  // Indexed by matrix[k][l]
  vector<vector<int>> matrix;
  for(int i=0;i<n_individuals;i++)
  {
    portable_getline(phase_file, line);
    vector<string> words = split(line, '\t');
    words.erase(words.begin());
    vector<int> loci;
    for(const auto& word: words)
      loci.push_back(convertTo<int>(word));
    matrix.push_back(loci);
  }

  assert(matrix[0].size() == 2*n_loci);

  OVector result;
  for(int l=0;l<n_loci;l++)
  {
    OVector locus;
    for(int i=0;i<n_individuals;i++) {
      locus.push_back(Int(matrix[i][2*l]));
      locus.push_back(Int(matrix[i][2*l+1]));
    }
    result.push_back(locus);
  }

  return result;
}

extern "C" closure builtin_function_remove_2nd_allele(OperationArgs& Args)
{
  object_ptr<const OVector> alleles = Args.evaluate_as<OVector>(0);

  OVector alleles2;

  for(int i=0;i<alleles->size();i+=2)
    alleles2.push_back((*alleles)[i]);

  return alleles2;
}

extern "C" closure builtin_function_allele_frequency_spectrum(OperationArgs& Args)
{
  object_ptr<const OVector> alleles = Args.evaluate_as<OVector>(0);

  int n_individuals = alleles->size();
  assert(n_individuals > 0);

  // 1. Count the alleles of each type
  map<int,int> allele_counts;
  for(const auto& a: *alleles)
  {
    int aa = *convert<const Int>(a);
    allele_counts[aa]++;
  }

  // 2. Determine how many alleles with each count there are.
  vector<int> spectrum(n_individuals,0);
  for(const auto& allele_and_count: allele_counts)
    spectrum[allele_and_count.second-1]++;
  
  // 3. Convert vector<int> to OVector
  OVector afs;
  for(int count: spectrum)
    afs.push_back(Int(count));

  return afs;
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

extern "C" closure builtin_function_ewens_sampling_group_probability(OperationArgs& Args)
{
  const double theta = *Args.evaluate_as<Double>(0);
  const vector<Vector<int>>& afs = *Args.evaluate_as<Vector<Vector<int>>>(1);

  log_double_t Pr = 1;
  for(const auto& a: afs)
    Pr *= ewens_sampling_probability(theta,a);

  return Log_Double(Pr);
}

extern "C" closure builtin_function_ewens_sampling_probability(OperationArgs& Args)
{
  const double theta = *Args.evaluate_as<Double>(0);
  object_ptr<const OVector> afs_ = Args.evaluate_as<OVector>(1);

  vector<int> afs;
  for(const auto& count: *afs_)
    afs.push_back(*convert<const Int>(count));

  log_double_t Pr = ewens_sampling_probability(theta,afs);

  return Log_Double(Pr);
}

extern "C" closure builtin_function_ewens_sampling_mixture_probability(OperationArgs& Args)
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
      pr += ps[i] * ewens_sampling_probability(thetas[i],a);

    Pr *= pr;
  }

  return Log_Double(Pr);
}

// The probability should be theta/(theta+total) is its new, and n/(theta+total) otherwise.
// Here the total and the count do not include the current allele;
double process_allele(int& count, int& total, double theta)
{
  double Pr;

  if (count == 0)
    Pr = theta/(theta + total);
  else if ( count > 0 )
    Pr = double(count)/(theta + total);
  else
    throw myexception()<<"GEM process: counts should not be negative!";

  count++;
  total++;

  return Pr;
}

extern "C" closure builtin_function_ewens_diploid_probability(OperationArgs& Args)
{
  // This is the theta = 2*N*mu
  const double theta = *Args.evaluate_as<Double>(0);

  assert(theta > 0);

  // These are indicators of coalescence
  object_ptr<const OVector> I_ = Args.evaluate_as<OVector>(1);
  const vector<object_ref>& I = *I_;

  // These are the alleles
  object_ptr<const OVector> alleles_ = Args.evaluate_as<OVector>(2);
  const vector<object_ref>& alleles = *alleles_;

  // How many times has each allele been seen?
  map<int,int> counts;

  // Determine number of individuals
  int n = alleles.size();
  assert(n%2 == 0);
  n /= 2;
  assert(n == I.size());

  double Pr = 1.0;
  for(int i=0,total=0;i<n;i++)
  {
    int a1 = *convert<const Int>(alleles[2*i]);
    int a2 = *convert<const Int>(alleles[2*i+1]);

    bool coalesced = convert<const constructor>(I[i])->f_name == "Prelude.True";

    // Heterozygotes coalesce before outbreeding with probability 0.
    if (a1 != a2 and coalesced)
      Pr = 0.0;

    Pr *= process_allele(counts[a1], total, theta);

    // Don't count the second allele if they coalesced.
    if (coalesced) continue;

    Pr *= process_allele(counts[a2], total, theta);
  }

  return Log_Double(Pr);
}

