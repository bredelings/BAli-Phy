#include <string>
#include <vector>
#include "popgen.H"
#include "computation/operation.H"
#include "computation/computation.H"
#include "computation/prelude.H"
#include "io.H"

using std::string;
using std::vector;

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
  Vector<vector<int>> result;
  for(int i=0;i<n_individuals;i++)
  {
    portable_getline(phase_file, line);
    vector<string> words = split(line, '\t');
    words.erase(words.begin());
    vector<int> loci;
    for(const auto& word: words)
      loci.push_back(convertTo<int>(word));
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
  object_ptr<const Vector<vector<int>>> alleles = Args.evaluate_as<Vector<vector<int>>>(0);

  Vector<vector<int>> alleles2 = *alleles;

  for(auto& v:alleles2.t)
    for(int i=v.size()-1;i>=0;i-=2)
      v.erase(v.begin() + i);

  return alleles2;
}

Program PopGen_Functions()
{
  Program P("PopGen");
  P.import_module(get_Prelude(), "Prelude", false);
  P.def_function("readPhaseFile", 1, lambda_expression(Read_PHASE_File()));
  P.def_function("remove2ndAllele", 1, lambda_expression(Remove_2nd_Allele()));

  return P;
}
