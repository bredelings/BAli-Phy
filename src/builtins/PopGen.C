#include <string>
#include <vector>
#include <map>
#include <unordered_map>
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

const int missing = -1;

extern "C" closure builtin_function_read_phase_file(OperationArgs& Args)
{
  const string& filename = Args.evaluate(0).as_<String>();

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
    string individual_name = words[0];
    words.erase(words.begin());
    vector<int> loci;
    for(const auto& word: words)
    {
      if (word == "NA")
      {
	loci.push_back(-1);
	continue;
      }

      int allele = convertTo<int>(word);
      if (allele <= 0)
	throw myexception()<<"read_phase_file: Allele values must be > 0, but read allele '"<<allele<<"'!\n  Use \"NA\" to indicate missing data.";

      loci.push_back(allele);
    }
    if (loci.size() != 2*n_loci)
      throw myexception()<<"Reading file '"<<filename<<"': expecting "<<2*n_loci<<" alleles for individual '"<<individual_name<<"', but actually got "<<loci.size()<<".";
    matrix.push_back(loci);
  }

  assert(matrix[0].size() == 2*n_loci);

  EVector result;
  for(int l=0;l<n_loci;l++)
  {
    EVector locus;
    for(int i=0;i<n_individuals;i++) {
      locus.push_back(matrix[i][2*l]);
      locus.push_back(matrix[i][2*l+1]);
    }
    result.push_back(locus);
  }

  return result;
}

extern "C" closure builtin_function_remove_2nd_allele(OperationArgs& Args)
{
  const EVector& alleles = Args.evaluate(0).as_<EVector>();

  EVector alleles2;

  for(int i=0;i<alleles.size();i+=2)
    alleles2.push_back(alleles[i]);

  return alleles2;
}

extern "C" closure builtin_function_allele_frequency_spectrum(OperationArgs& Args)
{
  const EVector& alleles = Args.evaluate(0).as_<EVector>();

  int n_individuals = alleles.size();
  assert(n_individuals > 0);

  // 1. Count the alleles of each type
  std::unordered_map<int,int> allele_counts;
  for(const auto& a: alleles)
  {
    int aa = a.as_int();
    allele_counts[aa]++;
  }

  // 2. Determine how many alleles with each count there are.
  vector<int> spectrum(n_individuals,0);
  for(const auto& allele_and_count: allele_counts)
    spectrum[allele_and_count.second-1]++;
  
  // 3. Convert vector<int> to EVector
  EVector afs;
  for(int count: spectrum)
    afs.push_back(count);

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
    Pr *= (double(i)/(theta+(i-1)));
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
  const double theta = Args.evaluate(0).as_double();
  const vector<Vector<int>>& afs = Args.evaluate(1).as_<Vector<Vector<int>>>();

  log_double_t Pr = 1;
  for(const auto& a: afs)
    Pr *= ewens_sampling_probability(theta,a);

  return {Pr};
}

extern "C" closure builtin_function_ewens_sampling_probability(OperationArgs& Args)
{
  const double theta = Args.evaluate(0).as_double();
  const EVector& afs_ = Args.evaluate(1).as_<EVector>();

  vector<int> afs;
  for(const auto& count: afs_)
    afs.push_back(count.as_int());

  log_double_t Pr = ewens_sampling_probability(theta,afs);

  return {Pr};
}

extern "C" closure builtin_function_ewens_sampling_mixture_probability(OperationArgs& Args)
{
  const vector<double>& thetas = Args.evaluate(0).as_<Vector<double>>();
  const vector<double>& ps = Args.evaluate(1).as_<Vector<double>>();
  const vector<Vector<int>>& afs = Args.evaluate(2).as_<Vector<Vector<int>>>();

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

  return {Pr};
}

// The probability should be theta/(theta+total) is its new, and n/(theta+total) otherwise.
// Here the total and the count do not include the current allele;
double process_allele(int& count, int& total, int& n_theta_pow, double theta)
{
  double Pr;

  if (theta < total)
  {
    if (count == 0)
    {
      Pr = 1.0/(theta + total);
      n_theta_pow++;
    }
    else
      Pr = double(count)/(theta + total);
  }
  else
  {
    if (count == 0)
      Pr = 1.0/(1.0 + total/theta);
    else if (count > 0)
    {
      n_theta_pow--;
      Pr = double(count)/(1.0 + total/theta);
    }
  }

  if (count < 0) throw myexception()<<"GEM process: counts should not be negative!";
  
  count++;
  total++;

  return Pr;
}

extern "C" closure builtin_function_ewens_diploid_probability(OperationArgs& Args)
{
  // 0. This is the theta = 2*N*mu
  const double theta = Args.evaluate(0).as_double();

  const int missing = 0;

  assert(theta > 0);

  // 1. These are indicators of coalescence
  const vector<expression_ref>& I = Args.evaluate(1).as_<EVector>();

  // 2. These are the alleles
  const vector<expression_ref>& alleles = Args.evaluate(2).as_<EVector>();

  // How many times has each allele been seen?
  std::unordered_map<int,int> counts;

  // Determine number of individuals
  int n = alleles.size();
  assert(n%2 == 0);
  n /= 2;
  assert(n == I.size());

  double Pr = 1.0;
  log_double_t Pr2 = 1.0;
  int n_theta_pow = 0;
  for(int i=0,total=0;i<n;i++)
  {
    if (Pr < 1.0e-30)
    {
      Pr2 *= Pr;
      Pr = 1.0;
    }

    int a1 = alleles[2*i].as_int();
    int a2 = alleles[2*i+1].as_int();

    int s1 = (a1 != missing)?1:0;
    int s2 = (a2 != missing)?1:0;
    int s = s1 + s2;

    if (s == 0)
      continue;
    else if (s == 1)
    {
      if (a1 == missing) std::swap(a1,a2);
      Pr *= process_allele(counts[a1], total, n_theta_pow, theta);
    }
    else 
    {
      assert(s == 2);
      bool coalesced = ( I[i].as_int() == 1);
      bool heterozygote = (a1 != a2);

      // Heterozygotes coalesce before outbreeding with probability 0.
      if (heterozygote and coalesced)
	return {log_double_t(0.0)};           // We *could* do Pr = 0.0 to accumulate zeros, thus penalizing them.

      Pr *= process_allele(counts[a1], total, n_theta_pow, theta);

      // Don't count the second allele if they coalesced.
      if (coalesced) continue;

      Pr *= process_allele(counts[a2], total, n_theta_pow, theta);
    }
  }

  Pr2 *= Pr * pow(log_double_t(theta), n_theta_pow);
  
  assert(Pr > 0.0);
  assert(Pr2 > 0.0);
  
  return {Pr2};
}

// Pr(I|s) = \sum_t=0^\infty s^t (1-s) (1/2^t)^(L-n) (1-(1/2^t))^n
extern "C" closure builtin_function_selfing_coalescence_probability(OperationArgs& Args)
{
  // The number of loci
  int L = Args.evaluate(0).as_int();

  // The selfing rate
  const double s = Args.evaluate(1).as_double();

  assert(s >= 0 and s <= 1);

  // These are indicators of coalescence
  auto arg2 = Args.evaluate(2);
  const vector<expression_ref>& I = arg2.as_<EVector>();

  // Determine number of coalescences;
  int n = 0;
  for(int l=0;l<L;l++)
  {
    bool coalesced = ( I[l].as_int() == 1);
    if (coalesced)
      n++;
  }

  // Handle s == 0
  if (s == 0.0)
  {
    if (n == 0)
      return {log_double_t(1.0)};
    else
      return {log_double_t(0.0)};
  }


  double sum = (n==0)?1.0:0.0;
  const double x1 = s*pow(0.5,L-n);
  double x2 = 1.0;
  double x3 = 1.0;

  for(int t = 1;; t++)
  {
    x2 *= x1;
    x3 *= 0.5;

    double delta = x2 * exp(log1p(-x3)*n);
    sum += delta;

    if (delta/sum < 1.0e-15 and t > 30)
      break;
  }

  sum *= (1.0 - s);

  return {log_double_t(sum)};
}

