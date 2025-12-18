#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <optional>
#include "computation/machine/args.H"
#include "util/io.H"
#include "util/string/split.H"
#include "util/string/strip.H"
#include "util/log-level.H"
#include "math/logprod.H"

using std::string;
using std::optional;
using std::vector;
using std::map;

// Remove anything after '#' (inclusive), but don't remove a '#' in column 0.
string phase_remove_comment(const string& line)
{
    if (line.size() == 0) return line;

    if (auto pos = line.find('#',1); pos != string::npos)
        return line.substr(0,pos);

    return line;
}

string get_phase_line(std::istream& i)
{
    string line;
    portable_getline(i, line);

    // If the read operation failed, throw an exception.
    // -> It would be nice to mention the line number and filename.
    if (not i)
        throw myexception()<<"Failure reading line of Phase file";

    // Remove comments
    line = phase_remove_comment(line);

    // Remove terminal whitespace
    line = rstrip(line," \t");

    return line;
}

template <typename T> 
T get_line_of(std::istream& i)
{
    return convertTo<T>( get_phase_line( i ) );
}

// I think this is the format for PHASE version 1.
extern "C" closure builtin_function_read_phase_file(OperationArgs& Args)
{
    const string& filename = Args.evaluate(0).as_<String>();

    checked_ifstream phase_file(filename,"PHASE v1 Input file");

    // Line 1
    int n_individuals = get_line_of<int>(phase_file);

    // Line 2
    int n_loci = get_line_of<int>(phase_file);

    // Line 3
    string line = get_phase_line(phase_file);

    if (line.size() != n_loci)  throw myexception()<<"read_phase_file: Loci description has "<<n_loci<<" loci, but header says there are "<<line.size()<<":\n  "<<line<<"\n";

    for(int i=0;i<line.size();i++)
	if (line[i] != 'M')
	    throw myexception()<<"Locus "<<i+1<<" is not a microsatellite locus!";

    // Lines 4- for each individual.
    // Indexed by matrix[k][l]
    vector<vector<int>> matrix;
    for(int i=0;i<n_individuals;i++)
    {
	line = get_phase_line(phase_file);
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

optional<string> read_S(const string& line, int& pos)
{
    while(pos < line.size() and (line[pos] == ' ' or line[pos] == '\t'))
        pos++;
    if (pos >= line.size()) return {};

    return line.substr(pos++,1);
}

optional<string> read_M(const string& line, int& pos)
{
    while(pos < line.size() and (line[pos] == ' ' or line[pos] == '\t'))
        pos++;
    if (pos >= line.size()) return {};

    int pos1 = pos;
    while(pos < line.size() and (line[pos] != ' ' and line[pos] != '\t'))
        pos++;

    return line.substr(pos1,pos-pos1);
}

optional<string> read_next(const string& line, char type, int& pos)
{
    if (type == 'M')
        return read_M(line,pos);
    else if (type == 'S')
        return read_S(line,pos);
    else
        throw myexception()<<"I don't recognize type '"<<type<<"'";
}


vector<string> split_characters(const string& line, const vector<char>& types)
{
    int pos = 0;

    vector<string> characters;
    for(int i=0;i<types.size();i++)
    {
        auto word = read_next(line, types[i], pos);
        if (word)
            characters.push_back(*word);
        else
            throw myexception()<<"Failed to read character '"<<i+1<<"!";
    }
    if (pos != line.size())
        throw myexception()<<"Extra characters after the end in line:\n  "<<line<<"\n";
    return characters;
}

int convert_character(const string& word, char type)
{
    if (type == 'M')
    {
        if (word == "NA")
            return -1;
        return convertTo<int>(word);
    }
    else if (type == 'S')
    {
        if (word == "0")
            return 0;
        else if (word == "1")
            return 1;
        else if (word == "A" or word == "a")
            return 0;
        else if (word == "C" or word == "c")
            return 1;
        else if (word == "G" or word == "g")
            return 2;
        else if (word == "T" or word == "t")
            return 3;
        else if (word == "?" or word == "-" or word == "N" or word == "n")
            return -1;
        else
            throw myexception()<<"I don't understand character '"<<word<<"' of type '"<<type<<"'\n";
    }

    std::abort();
}

vector<int> convert_characters(const vector<string>& words, const vector<char>& types)
{
    assert(words.size() == types.size());
    vector<int> characters(words.size());
    for(int i=0;i<words.size();i++)
        characters[i] = convert_character(words[i], types[i]);
    return characters;
}


/* Sample fastPhase 1.4
// http://scheet.org/code/fastphase_doc_1.4.pdf

no.individuals
no.SNPsites
P pos(1) pos(2) ... pos(no.SNPsites) <optional line>
SSS...SSS <optional line>
ID (1)
genotypes(1-a)
genotypes(1-b)
ID (2)
genotypes(2-a)
genotypes(2-b)
...
ID (no.individuals)
genotypes(no.individuals-a)
genotypes(no.individuals-b)


3
4
# id 1
1a11
0t01
# id 2
1t11
0a00
# id 3
?a01
?t10
*/

/* Sample Phase v2.1 format
// http://stephenslab.uchicago.edu/assets/software/phase/instruct2.1.pdf

3
5
P 300 1313 1500 2023 5635
MSSSM
#1
12 1 0 1 3
11 0 1 0 3
#2
12 1 1 1 2
12 0 0 0 3
#3
-1 ? 0 0 2
-1 ? 1 1 13
*/

// For Phase v2, I'm only going to implement SNP sites for now.
extern "C" closure builtin_function_read_phase2_file(OperationArgs& Args)
{
    const string& filename = Args.evaluate(0).as_<String>();

    checked_ifstream phase_file(filename,"PHASE v2 Input file");

    // INDIVIDUALS (Line 1)
    int n_individuals = get_line_of<int>(phase_file);
    if (log_verbose) std::cerr<<"read_phase2_file: "<<n_individuals<<" individuals\n";

    // LOCI (Line 2)
    int n_loci = get_line_of<int>(phase_file);
    if (log_verbose) std::cerr<<"read_phase2_file: "<<n_loci<<" loci\n";

    // POSITIONS (line 3 - optional)
    string line;
    if (int c = phase_file.peek(); c == 'P')
    {
        line = get_phase_line(phase_file);
        if (log_verbose) std::cerr<<"read_phase2_file: skipping P line\n";
    }

    // TYPE (OPTIONAL)
    vector<char> types(n_loci);
    if (int c = phase_file.peek(); c != '#' and c != EOF)
    {
        line = get_phase_line(phase_file);

        if (line.size() != n_loci)  throw myexception()<<"read_phase2_file: Line 2 declares "<<n_loci<<" loci, but type line says there are "<<line.size()<<":\n  "<<line<<"\n";
        for(int i=0;i<line.size();i++)
        {
            types[i] = line[i];
            if (types[i] != 'S' and types[i] != 'M')
                throw myexception()<<"read_phase2_file: locus "<<i+1<<" has unrecognized type '"<<types[i]<<":\n  "<<line<<"\n";
        }
    }
    else
    {
        if (log_verbose) std::cerr<<"read_phase2_file: No type line - assuming all 'S'\n";
        for(int i=0;i<types.size();i++)
            types[i] = 'S';
    }

    // Lines 4- for each individual.
    // Indexed by matrix[k][l]
    vector<vector<int>> matrix;
    for(int i=0;i<n_individuals;i++)
    {
	auto name = get_phase_line(phase_file);
        if (not name.size() or name[0] != '#')
            throw myexception()<<"Expected to find name for individual "<<i+1<<" but got line:\n  "<<name<<"\n";
        name = lstrip(name.substr(1)," \t");
        if (log_verbose) std::cerr<<"read_phase2_file: individual '"<<name<<"' ";

        // SInce we only allow SNPs, we can ignore spaces
        auto data1 = get_phase_line(phase_file);
        if (log_verbose) std::cerr<<".";
        auto data2 = get_phase_line(phase_file);
        if (log_verbose) std::cerr<<".";

        auto words1 = split_characters(data1, types);
        if (log_verbose) std::cerr<<".";
        auto words2 = split_characters(data2, types);
        if (log_verbose) std::cerr<<".";

        matrix.push_back(convert_characters(words1, types));
        if (log_verbose) std::cerr<<".";
        matrix.push_back(convert_characters(words2, types));
        if (log_verbose) std::cerr<<".";
        if (log_verbose) std::cerr<<" done.\n";
    }

    assert(matrix.size() == 2*n_individuals);
    assert(matrix[0].size() == n_loci);

    EVector result;
    for(int l=0;l<n_loci;l++)
    {
	EVector locus;
	for(int i=0;i<n_individuals;i++) {
	    locus.push_back(matrix[2*i][l]);
	    locus.push_back(matrix[2*i+1][l]);
	}
	result.push_back(locus);
    }

    return result;
}

extern "C" closure builtin_function_remove_2nd_allele(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const EVector& alleles = arg0.as_<EVector>();

    EVector alleles2;

    for(int i=0;i<alleles.size();i+=2)
	alleles2.push_back(alleles[i]);

    return alleles2;
}

extern "C" closure builtin_function_allele_frequency_spectrum(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const EVector& alleles = arg0.as_<EVector>();

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
    auto arg1 = Args.evaluate(1);
    const EVector& afs_ = arg1.as_<EVector>();

    vector<int> afs;
    for(const auto& count: afs_)
	afs.push_back(count.as_int());

    log_double_t Pr = ewens_sampling_probability(theta,afs);

    return {Pr};
}

extern "C" closure builtin_function_ewens_sampling_mixture_probability(OperationArgs& Args)
{
    auto thetas = (vector<double>) Args.evaluate(0).as_<EVector>();
    auto ps =     (vector<double>) Args.evaluate(1).as_<EVector>();
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
	log_double_t pr = 0;
	for(int i=0;i<thetas.size();i++)
	    pr += ps[i] * ewens_sampling_probability(thetas[i],a);

	Pr *= pr;
    }

    return {Pr};
}

// The probability should be theta/(theta+total) if it is new, and n/(theta+total) otherwise.
// Here the total and the count do not include the current allele;
double process_allele(int& count, int& total, int& n_theta_pow, double theta)
{
    double Pr = 0;

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

    log_prod_underoverflow Pr;
    int n_theta_pow = 0;
    for(int i=0,total=0;i<n;i++)
    {
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
	    {
		Pr *= 0.0;  // This accumulates zeros, in order to penalize them.
		continue;
	    }

	    Pr *= process_allele(counts[a1], total, n_theta_pow, theta);

	    // Don't count the second allele if they coalesced.
	    if (coalesced) continue;

	    Pr *= process_allele(counts[a2], total, n_theta_pow, theta);
	}
    }

    Pr *= pow(log_double_t(theta), n_theta_pow);
  
    return expression_ref{Pr};
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

