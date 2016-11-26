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

///
/// \file setup-smodel.C
///
/// \brief Create substitution models from strings of the form model[arg,arg]+...+model[arg,arg].
///


// TODO:
// 1. Why aren't the frequency models in the default_arguments list?
//
// 2. When the user supplies HKY, report this as HKY+F (somehow).
//   - We could either use typed arguments to correct this BEFORE processing (faster).
//     + So the coerce functions would work on the strings instead
//     + And we need a rule to make +F evaluate to frequencies, but HKY+F evaluate to a rate matrix.
//       (See process_stack_frequencies).
//       (I guess this means that gwF[] and gwF[arg] have different types!
//         + For ALL frequency models!
//
// 3. Remove setting of frequencies from the construction of the model.
//   - Could this allow us to determine the alphabet from the model, so that we don't need to
//     write --smodel=M0 --alphabet=Codons?
//   - When no smodel is supplied, we would still have to try parsing the alignment
//   - Also, codons of DNA, or codons of RNA?

//   - So, maybe an INITIAL parse, only to DNA, RNA, or AA, just to determine alphabet?
//   - Then 


#include <vector>
#include <boost/program_options.hpp>
#include "smodel/setup.H"
#include "util.H"
#include "myexception.H"
#include "computation/module.H"
#include "computation/loader.H"
#include <boost/property_tree/ptree.hpp>
#include <boost/optional.hpp>
#include <boost/property_tree/info_parser.hpp>
#include "computation/model_expression.H"

using boost::property_tree::ptree;
using boost::optional;
using std::string;
using std::pair;
using std::vector;
using std::valarray;
using boost::program_options::variables_map;
using boost::shared_ptr;

const vector<vector<string>> all_default_arguments = 
{
    {"EQU"},
    {"F81"},
    {"HKY","kappa"},
    {"TN","kappaPur","kappaPyr"},
    {"GTR","ag","at","ac","gt","gc","tc"},
    {"HKYx3","kappa"},
    {"TNx3","kappaPur","kappaPyr"},
    {"GTRx3","ag","at","ac","gt","gc","tc"},
    {"PAM"},
    {"JTT"},
    {"WAG"},
    {"LG"},
    {"Empirical","filename"},
    {"M0","submodel=HKY"},
    {"fMutSel","*submodel"},
    {"fMutSel0","*submodel"},
    {"INV","p"},
    {"DP","n","*submodel"},
    {"gamma","n=4","alpha","*submodel"},
    {"gamma_inv","n=4","alpha","p","*submodel"},
    {"log-normal","n=4","sigmaOverMu","*submodel"},
    {"log-normal_inv","n=4","sigmaOverMu","p","*submodel"},
    {"M1a","nuc_model=HKY","freq_model=F61"},
    {"M2a","nuc_model=HKY","freq_model=F61"},
    {"M2a_Test","nuc_model=HKY","freq_model=F61"},
    //    {"M3u","3","nuc_model="HKY","freq_model=F61"},
    {"M3","n=4","nuc_model=HKY","freq_model=F61"},
    {"M3_Test","n=4","nuc_model=HKY","freq_model=F61"},
    {"M7","n=4","nuc_model=HKY","freq_model=F61"},
    {"M8","n=4","nuc_model=HKY","freq_model=F61"},
    {"M8a","n=4","nuc_model=HKY","freq_model=F61"},
    {"M8a_Test","n=4","nuc_model=HKY","freq_model=F61"},
    {"branch-site","n=2","nuc_model=HKY","freq_model=F61"},
    {"dp_omega","n=4","nuc_model=HKY","freq_model=F61"}
};

optional<pair<string,string>> split_keyword(const string& s)
{
    int pos = s.find('=');
    if (pos == -1)
	return boost::none;
    else
	return pair<string,string>({s.substr(0,pos),s.substr(pos+1)});
}

string show(const ptree& pt, int depth = 0)
{
    string result = "";
    string indent(depth,' ');
    string indent2(depth+2,' ');
    result += "'"+pt.get_value<string>()+"'\n";
    for(auto c: pt)
    {
	result += indent2 + c.first + " : ";
	result += show(c.second,depth+4);
    }
    return result;
}

string show(vector<string> args)
{
    string output = args[0];
    args.erase(args.begin());
    if (args.empty()) return output;

    output += '[';
    output += join(args,',');
    output += ']';
    return output;
}

string get_keyword_for_positional_arg(const string& head, int i)
{
    for(const auto& default_arguments: all_default_arguments)
    {
	if (default_arguments[0] != head) continue;

	if (i+1 >= default_arguments.size())
	    throw myexception()<<"Trying to access positional arg "<<i+1<<" for '"<<head<<"', which only has "<<default_arguments.size()-1<<" positional arguments.";

	// Strip leading '*' that indicates required argument
	string keyword = default_arguments[i+1];
	if (keyword[0] == '*')
	    keyword = keyword.substr(1);
	auto keyword_pair = split_keyword(keyword);
	if (keyword_pair)
	    return (*keyword_pair).first;
	else
	    return keyword;
    }
    throw myexception()<<"No positional arguments for '"<<head<<"'!";
}

void set_default_values(ptree& args)
{
    const string& head = args.get_value<string>();
  
    for(const auto& default_arguments: all_default_arguments)
    {
	if (default_arguments[0] != head) continue;

	for(int i=0;i<default_arguments.size()-1;i++)
	{
	    string keyword = default_arguments[i+1];
	    if (keyword[0] == '*')
		keyword = keyword.substr(1);
	    auto keyword_pair = split_keyword(keyword);
	    if (not keyword_pair) continue;
      
	    if (not args.count(keyword_pair->first))
		args.push_back({keyword_pair->first, ptree(keyword_pair->second)});
	}
    }
}

void check_required_args(const ptree& args)
{
    //  std::cout<<"checkout args:\n";
    //  write_info(std::cout, args);
    //  std::cout<<show(args)<<std::endl;
    const string& head = args.get_value<string>();
  
    for(const auto& default_arguments: all_default_arguments)
    {
	if (default_arguments[0] != head) continue;

	for(int i=0;i<default_arguments.size()-1;i++)
	{
	    string keyword = default_arguments[i+1];
	    if (keyword[0] != '*') continue;
	    keyword = keyword.substr(1);

	    if (not args.count(keyword))
		throw myexception()<<"Command '"<<head<<"' missing required argument '"<<keyword<<"'";
	}
    }
}

// Turn an expression of the form head[arg1, arg2, ..., argn] -> {head, arg1, arg2, ..., argn}.
vector<string> split_args(string s)
{
    vector<string> args;

    // 1. Get the head
    int pos = s.find('[');
    if (pos == -1)
    {
	args = {s};
	return args;
    }

    args = { s.substr(0,pos) };
    s = s.substr(pos);

    //2. Get the arguments from '[arg1, arg2, ... , argn]'.
    int depth = 0;
    int start = 1;
    for(int i=0;i<s.size();i++)
    {
	// Record finished arg.
	if ((s[i] == ']' or s[i] == ',') and depth == 1)
	{
	    assert(i >= start);
	    args.push_back(s.substr(start,i-start));
	    start = i+1;
	}

	if (s[i] == '[')
	    depth++;
	else if (s[i] == ']')
	{
	    depth--;
	    if (depth < 0) throw myexception()<<"Malformed expression '"<<s<<"': ']' has no matching '['";
	}
    }
    if (depth > 0) throw myexception()<<"Malformed expression '"<<s<<"': missing ']'";

    return args;
}

/// \brief Turn an expression of the form h1[a]+h2[b] -> h2[h1[a],b].
///
/// \param sstack A stack of strings that represent a substitution model.
/// \param s The model name to match.
/// \param args The possible argument.
///
pair<optional<string>,string> split_last_plus(const string& s)
{
    // 1. Find last '+' on the top level
    int split = -1;
    int depth = 0;
    for(int i=0;i<s.size();i++)
	if (s[i] == '[')
	    depth++;
	else if (s[i] == ']')
	{
	    depth--;
	    if (depth < 0) throw myexception()<<"Too many ']' in string '"<<s<<"'";
	}
	else if (depth == 0 and s[i] == '+')
	    split = i;
    if (depth != 0)
	throw myexception()<<"Too many '[' in string '"<<s<<"'";
  
    // 2. If there are no plus expressions then we can take the string as is.
    if (split == -1)
	return {boost::none, s};
    // 3. Otherwise divide the string on the last plus
    else
	return {s.substr(0,split), s.substr(split+1)};
}

/// Split a string of the form key=value into {key,value}
ptree parse(const string& s);

/// Parse strings of the form head[value1, value2, key3=value3, ...]
ptree parse_no_submodel(const string& s)
{
    // 1. Split the head and the arguments
    auto args = split_args(s);

    // 2. Set the head
    string head = args.front();
    args.erase(args.begin());

    ptree result;
    result.put_value(head);
  
    // 3. Attempt to set the supplied arguments
    bool seen_keyword_arg = false;
    for(int i=0;i<args.size();i++)
    {
	pair<string,string> key_value;

	// 4. Ignore empty arguments
	if (args[i].empty()) continue;

	// 5. If we have a keyword argument, remember it
	if (auto arg = split_keyword(args[i]))
	{
	    seen_keyword_arg = true;
	    key_value = *arg;
	}
	// 6. Otherwise find the keyword for the positional argument
	else
	{
	    if (seen_keyword_arg)
		throw myexception()<<"Positional argument after keyword argument in '"<<s<<"'";
	    key_value = {get_keyword_for_positional_arg(head, i), args[i]};
	}

	// 7. Set the key = value after parsing the value.
	if (result.count(key_value.first))
	    throw myexception()<<"Trying to set value for "<<head<<"."<<key_value.first<<" a second time!";
	result.push_back({key_value.first, parse(key_value.second)});
    }

    return result;
}

// Parse strings of the form head[args] + head[args] + ... + head[args]
ptree parse(const string& s)
{
    // 1. Get the last head[args]
    auto ss = split_last_plus(s);

    // 2. Parse the last head[args]
    auto result = parse_no_submodel(ss.second);

    // 3. Parse the remainder and add it as a "submodel" argument
    if (ss.first)
    {
	if (result.count("submodel"))
	    throw myexception()<<"Trying to specify a submodel with '+' when submodel already specified by keyword!";
	result.push_back({"submodel",parse(*ss.first)});
    }

    // 4. Set default values
    set_default_values(result);
  
    // 5. Check that required arguments are specified
    check_required_args(result);

    return result;
}

/// \brief Return the default substitution model name for alphabet \a a, and "" if there is no default.
string default_markov_model(const alphabet& a) 
{
    if (dynamic_cast<const Nucleotides*>(&a))
	return "TN";
    else if (dynamic_cast<const AminoAcidsWithStop*>(&a))
	return "";
    else if (dynamic_cast<const AminoAcids*>(&a))
	return "LG";
    else if (dynamic_cast<const Codons*>(&a))
	return "M0";
    else if (dynamic_cast<const Triplets*>(&a))
	return "TNx3";
    else
	return "";
}

expression_ref
get_smodel_(const module_loader& L,const ptree& model_rep,const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >&);

expression_ref
get_smodel_(const module_loader& L,const ptree& model_rep,const object_ptr<const alphabet>& a);

expression_ref
get_smodel_(const module_loader& L,const ptree& model_rep);

/// \brief Construct an AlphabetExchangeModel from string \a smodel.
expression_ref coerce_to_EM(const module_loader& L,
			    const ptree& model_rep,
			    const object_ptr<const alphabet>& a, 
			    const shared_ptr< const valarray<double> >& frequencies)

{
    if (model_rep.empty() and model_rep.data().empty())
    {
	std::cout<<show(model_rep)<<std::endl;
	throw myexception()<<"Can't construct substitution model from empty description!";
    }

    expression_ref S = get_smodel_(L, model_rep, a, frequencies);

    if (S and result(S,L,vector<string>{"SModel","Distributions","Range"}).is_a<Box<Matrix>>())
	return S;

    throw myexception()<<": '"<<show(model_rep)<<"' is not an exchange model.";
}

/// \brief Construct a ReversibleMarkovModel from model \a M
expression_ref coerce_to_RA(const module_loader& L,
			    const expression_ref& M,
			    const object_ptr<const alphabet>& a)
{
    auto result = ::result(M, L, {"SModel", "Distributions","Range"});

    if (has_constructor(result, "SModel.F81"))
	return M;

    if (has_constructor(result, "SModel.ReversibleMarkov"))
	return M;

    try 
    {
	if (has_constructor(result, "SModel.ReversibleFrequency"))
	    throw myexception()<<"Cannot construct CTMC model from frequency model alone!";

	if (result.is_a<Box<Matrix>>())
	{
	    // If the frequencies.size() != alphabet.size(), this call will throw a meaningful exception.
	    expression_ref r = model_expression({identifier("plus_f_model"),*a});
	    expression_ref s = M;
	    expression_ref mm = model_expression({identifier("reversible_markov_model"), s, r});
	    return mm;
	}
	throw myexception()<<": Can't construct a SimpleReversibleMarkovModel from '"<<M<<"\n";
    }
    catch (std::exception& e) { 
	throw myexception()<<": Can't construct a SimpleReversibleMarkovModel from '"<<M<<"':\n "<<e.what();
    }
}

/// \brief Construct a ReversibleMarkovModel from model \a M
expression_ref coerce_to_RA(const module_loader& L,
			    const ptree& model_rep,
			    const object_ptr<const alphabet>& a,
			    const shared_ptr< const valarray<double> >& frequencies)
{
    expression_ref M = get_smodel_(L, model_rep, a, frequencies);

    return coerce_to_RA(L, M, a);
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
expression_ref process_stack_Markov(const module_loader& L,
				    const ptree& model_rep,
				    const object_ptr<const alphabet>& a)
{
    //------ Get the base markov model (Reversible Markov) ------//
    /*
      if (model_rep.get_value<string>() == "EQU")
      {
      return EQU_Model(*a);
      }
      else if (model_rep.get_value<string>() == "F81")
      {
      if (frequencies)
      return F81_Model(*a,*frequencies);
      else
      return F81_Model(*a);
      }
    */

    if (model_rep.get_value<string>() == "JC")
	return (identifier("jukes_cantor"),*a);
    else if (model_rep.get_value<string>() == "EQU")
    {
	const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
	if (not N)
	    throw myexception()<<"EQU: '"<<a->name<<"' is not a nucleotide alphabet.";

	return model_expression({identifier("equ_model"),*a});
    }
    else if (model_rep.get_value<string>() == "HKY")
    {
	const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
	if (not N)
	    throw myexception()<<"HKY: '"<<a->name<<"' is not a nucleotide alphabet.";

	if (model_rep.count("kappa"))
	{
	    double kappa = model_rep.get<double>("kappa");
	    return (identifier("hky"),*a,kappa);
	}
	else
	    return model_expression({identifier("hky_model"),*a});
    }
    else if (model_rep.get_value<string>() == "TN")
    {
	const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
	if (not N)
	    throw myexception()<<"TN: '"<<a->name<<"' is not a nucleotide alphabet.";

	if (model_rep.count("kappa_pur") and model_rep.count("kappa_pyr"))
	{
	    expression_ref kappa_pur = model_rep.get<double>("kappa_pur");
	    expression_ref kappa_pyr = model_rep.get<double>("kappa_pyr");
	    return (identifier("tn"), *a, kappa_pur, kappa_pur);
	}
	else
	    return model_expression({identifier("tn_model"),*a});
    }
    else if (model_rep.get_value<string>() == "GTR")
    {
	const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
	if (not N)
	    throw myexception()<<"GTR: '"<<a->name<<"' is not a nucleotide alphabet.";

	// FIXME - allow/make a general GTR model!

	return model_expression({identifier("gtr_model"),*a});
    }
    /*
      else if (model_rep.get_value<string>() == "EQUx3")) {

      const Triplets* T = dynamic_cast<const Triplets*>(&*a);
      if (T) 
      return Singlet_to_Triplet_Exchange(*T,EQU(T->getNucleotides()));
      else
      throw myexception()<<"EQUx3: '"<<a->name<<"' is not a triplet alphabet.";
      }
    */
    else if (model_rep.get_value<string>() == "HKYx3")
    {
	if (dynamic_cast<const Triplets*>(&*a))
	    return model_expression({identifier("hkyx3_model"),*a});
	else
	    throw myexception()<<"HKYx3: '"<<a->name<<"' is not a triplet alphabet.";
    }
    else if (model_rep.get_value<string>() == "TNx3")
    {
	if (dynamic_cast<const Triplets*>(&*a))
	    return model_expression({identifier("tnx3_model"),*a});
	else
	    throw myexception()<<"TNx3: '"<<a->name<<"' is not a triplet alphabet.";
    }
    else if (model_rep.get_value<string>() == "GTRx3")
    {
	if (dynamic_cast<const Triplets*>(&*a))
	    return model_expression({identifier("gtrx3_model"),*a});
	else
	    throw myexception()<<"GTRx3: '"<<a->name<<"' is not a triplet alphabet.";
    }
    else if (model_rep.get_value<string>() == "PAM")
    {
	if (*a != AminoAcids())
	    throw myexception()<<"PAM: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
	return (identifier("SModel.pam"),a);
    }
    else if (model_rep.get_value<string>() == "JTT") {
	if (*a != AminoAcids())
	    throw myexception()<<"JTT: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
	return (identifier("SModel.jtt"),a);
    }
    else if (model_rep.get_value<string>() == "WAG") {
	if (*a != AminoAcids())
	    throw myexception()<<"WAG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
	return (identifier("SModel.wag"),a);
    }
    else if (model_rep.get_value<string>() == "LG") {
	if (*a != AminoAcids())
	    throw myexception()<<"LG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
	return (identifier("SModel.lg"),a);
    }
    else if (model_rep.get_value<string>() == "Empirical") 
    {
	return (identifier("SModel.empirical"),a,model_rep.get<string>("filename"));
    }
    /*
      else if (model_rep.get_value<string>() == "C10")
      {
      if (*a != AminoAcids())
      throw myexception()<<"C20: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
      return C10_CAT_FixedFrequencyModel();
      }
      else if (model_rep.get_value<string>() == "C20")
      {
      if (*a != AminoAcids())
      throw myexception()<<"C20: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
      return C20_CAT_FixedFrequencyModel();
      }
      else if (model_rep.get_value<string>() == "CAT-Fix") {
      if (*a != AminoAcids())
      throw myexception()<<"CAT-Fix: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
      CAT_FixedFrequencyModel M(*a);
      M.load_file(arg);
      return M;
      }
    */
    else if (model_rep.get_value<string>() == "M0") //M0[S]
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<"M0: '"<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref S = coerce_to_EM(L, model_rep.get_child("submodel"), const_ptr(N), {});

	return model_expression({identifier("m0_model"), a , S});
    }
    else if (model_rep.get_value<string>() == "fMutSel")
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<"fMutSel: '"<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref nuc_rm = coerce_to_RA(L,model_rep.get_child("submodel"), const_ptr(N), {});

	return model_expression({identifier("fMutSel_model"), a , nuc_rm});
    }
    else if (model_rep.get_value<string>() == "fMutSel0")
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<"fMutSel0: '"<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref nuc_rm = coerce_to_RA(L,model_rep.get_child("submodel"), const_ptr(N), {});

	return model_expression({identifier("fMutSel0_model"), a , nuc_rm});
    }

    return {};
}

optional<vector<double>> get_frequencies_from_tree(const ptree& model_rep, const alphabet& a)
{
    vector<double> pi;
    for(int i=0;i<a.size();i++)
	if (model_rep.count(a.letter(i)))
	    pi.push_back(model_rep.get<double>(a.letter(i)));

    if (pi.size() > 0 and pi.size() < a.size())
    {
	string head = model_rep.get_value<string>();
	throw myexception()<<"For frequency model '"<<head<<"', you must specify all letter frequencies, or none!";
    }

    if (pi.empty())
	return boost::none;
    else
	return pi;
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
expression_ref process_stack_Frequencies(const module_loader& L,
					 const ptree& model_rep,
					 const object_ptr<const alphabet>& a,
					 const shared_ptr<const valarray<double> >& frequencies)
{
    expression_ref R;

    if (model_rep.get_value<string>() == "F=constant") 
    {
	if (not frequencies)
	    throw myexception()<<"F=constant: frequency estimates not available here.";

	Vector<double> v;
	v.resize(a->size());
	for(int i=0;i<a->size();i++)
	    v[i] = (*frequencies)[i];

	R = (identifier("ReversibleFrequency"), *a, (identifier("iotaUnsigned"), a->size()), v, (identifier("SModel.plus_gwF"), a, 1.0, v));
    }
    else if (model_rep.get_value<string>() == "F" or model_rep.get_value<string>() == "F61")
    {
	if (model_rep.get_value<string>() == "F61" and a->size() != 61)
	    throw myexception()<<"Cannot use 'F61' frequency model since alphabet contains "<<a->size()<<" letters.";

	if (auto pi = get_frequencies_from_tree(model_rep, *a))
	    R = (identifier("plus_f"), a, get_list(*pi));
	else
	    R = model_expression({identifier("plus_f_model"),a});
    }
    else if (model_rep.get_value<string>() == "gwF")
    {
	auto pi = get_frequencies_from_tree(model_rep, *a);
	if (pi and model_rep.count("f"))
	{
	    double f = model_rep.get<double>("f");
	    R = (identifier("plus_gwf"), a, get_list(*pi), f);
	}
	else
	    R = model_expression({identifier("plus_gwf_model"),a});
    }
    else if (model_rep.get_value<string>() == "F=uniform") 
	R = (identifier("uniform_f_model"),a);
    else if (model_rep.get_value<string>() == "F1x4")
    {
	if (not dynamic_cast<const Triplets*>(&*a))
	    throw myexception()<<"+F1x4: '"<<a->name<<"' is not a triplet alphabet.";

	if (auto nuc_pi = get_frequencies_from_tree(model_rep, dynamic_cast<const Triplets*>(&*a)->getNucleotides()))
	    R = (identifier("f1x4"), a, get_list(*nuc_pi));
	else
	    R = model_expression({identifier("f1x4_model"), a});
    }
    else if (model_rep.get_value<string>() == "F3x4") 
    {
	if (not dynamic_cast<const Triplets*>(&*a))
	    throw myexception()<<"+F1x4: '"<<a->name<<"' is not a triplet alphabet.";
	R = model_expression({identifier("f3x4_model"),a});
    }
    else if (model_rep.get_value<string>() == "MG94") 
    {
	if (not dynamic_cast<const Triplets*>(&*a))
	    throw myexception()<<"+MG94w9: '"<<a->name<<"' is not a triplet alphabet.";

	R = model_expression({identifier("mg94_model"),a});
    }
    else if (model_rep.get_value<string>() == "MG94w9") 
    {
	if (not dynamic_cast<const Triplets*>(&*a))
	    throw myexception()<<"+MG94w9: '"<<a->name<<"' is not a triplet alphabet.";

	R = model_expression({identifier("mg94w9_model"),a});
    }
    /*
      else if (model_rep.get_value<string>() == "F=amino-acids") 
      {
      const Codons* C = dynamic_cast<const Codons*>(&*a);
      if (not C)
      throw myexception()<<"+F=amino-acids: '"<<a->name<<"' is not a codon alphabet.";

      R = AACodonFrequencyModel(*C);
      }
      else if (model_rep.get_value<string>() == "F=triplets") 
      {
      const Triplets* T = dynamic_cast<const Triplets*>(&*a);
      if (not T)
      throw myexception()<<"+F=triplets: '"<<a->name<<"' is not a triplet alphabet.";

      R = TripletsFrequencyModel(*T);
      }
      else if (model_rep.get_value<string>() == "F=codons") 
      {
      const Codons* C = dynamic_cast<const Codons*>(&*a);
      if (not C)
      throw myexception()<<"+F=codons: '"<<a->name<<"' is not a codon alphabet.";

      R = CodonsFrequencyModel(*C);
      }
      else if (model_rep.get_value<string>() == "F=codons2") 
      {
      const Codons* C = dynamic_cast<const Codons*>(&*a);
      if (not C)
      throw myexception()<<"+F=codons2: '"<<a->name<<"' is not a codon alphabet.";

      R = CodonsFrequencyModel2(*C);
      }
    */

    if (R and model_rep.count("submodel"))
    {
	// If the frequencies.size() != alphabet.size(), this call will throw a meaningful exception.
	expression_ref s = coerce_to_EM(L,model_rep.get_child("submodel"), a, frequencies);
	expression_ref mm = model_expression({identifier("reversible_markov_model"), s, R});
	return mm;
    }

    return R;
}

expression_ref coerce_to_frequency_model(const module_loader& L,
					 const expression_ref& M,
					 const object_ptr<const alphabet>& /* a */,
					 const shared_ptr< const valarray<double> >& /* frequencies */)
{
    if (has_constructor(result(M,L,{"SModel","Distributions","Range"}), "SModel.ReversibleFrequency"))
	return M;

    throw myexception()<<": '"<<M<<"' is not an exchange model.";
}

expression_ref coerce_to_frequency_model(const module_loader& L,
					 const ptree& model_rep,
					 const object_ptr<const alphabet>& a,
					 const shared_ptr< const valarray<double> >& frequencies)
{
    expression_ref M = get_smodel_(L, model_rep, a, frequencies);

    return coerce_to_frequency_model(L, M, a,  frequencies);
}


/// \brief Construct a MultiModel from model \a M
expression_ref coerce_to_MM(const module_loader& L,
			    const expression_ref& M,
			    const object_ptr<const alphabet>& a)
{
    if (M and has_constructor(result(M, L,{"SModel","Distributions","Range"}), "SModel.MixtureModel"))
	return M;

    try { 
	expression_ref ra = coerce_to_RA(L,M,a);
	return model_expression({identifier("unit_model"), ra});
    }
    catch (std::exception& e) { 
	throw myexception()<<": Can't construct a MixtureModel from '"<<M<<"':\n"<<e.what();
    }
}

/// \brief Construct a MultiModel from model \a M
expression_ref coerce_to_MM(const module_loader& L,
			    const ptree& model_rep,
			    const object_ptr<const alphabet>& a, 
			    const shared_ptr< const valarray<double> >& frequencies)
{
    expression_ref M = get_smodel_(L, model_rep, a, frequencies);

    return coerce_to_MM(L, M, a);
}

/// \brief Construct a MultiModel from model \a M
expression_ref coerce_to_MMM(const module_loader& L,
			     const expression_ref& M,
			     const object_ptr<const alphabet>& a)
{
    if (has_constructor(result(M,L,{"SModel","Distributions","Range"}), "SModel.MixtureModels"))
	return M;

    try { 
	expression_ref mm = coerce_to_MM(L, M, a);
	return model_expression({identifier("mmm"), mm});
    }
    catch (std::exception& e) { 
	throw myexception()<<": Can't construct a MixtureModels from '"<<M<<"':\n"<<e.what();
    }
}

/// \brief Construct a MultiModel from model \a M
expression_ref coerce_to_MMM(const module_loader& L,
			     const ptree& model_rep,
			     const object_ptr<const alphabet>& a,
			     const shared_ptr< const valarray<double> >& frequencies)
{
    expression_ref M = get_smodel_(L, model_rep, a, frequencies);
  
    return coerce_to_MMM(L, M, a);
}


expression_ref process_stack_Multi(const module_loader& L,
				   const ptree& model_rep,
				   const object_ptr<const alphabet>& a,
				   const shared_ptr< const valarray<double> >& frequencies)
{
    if (model_rep.get_value<string>() == "single") 
	return coerce_to_MM(L,coerce_to_RA(L,model_rep.get_child("submodel"),a,frequencies),a);

    // else if (model_rep.get_value<string>() == "gamma_plus_uniform") {
    else if (model_rep.get_value<string>() == "gamma") 
    {
	expression_ref base = coerce_to_RA(L, model_rep.get_child("submodel"),a,frequencies);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("gamma_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "gamma_inv") 
    {
	expression_ref base = coerce_to_RA(L, model_rep.get_child("submodel"),a,frequencies);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("gamma_inv_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "INV") 
    {
	expression_ref base = coerce_to_RA(L, model_rep.get_child("submodel"), a,frequencies);

	return model_expression({identifier("inv_model"), base});
    }
    else if (model_rep.get_value<string>() == "log-normal") 
    {
	expression_ref base = coerce_to_RA(L, model_rep.get_child("submodel"),a,frequencies);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("log_normal_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "log-normal_inv") 
    {
	expression_ref base = coerce_to_RA(L, model_rep.get_child("submodel"),a,frequencies);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("log_normal_inv_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "multi_freq") {
	// Pr(l|m) = Pr(m|l)*Pr(l)/Pr(m)
	// Idea: store Pr(m|l) = probability a letter is in each model
	//       store Pr(l)   = overall frequencies for each letter
	//       store Pr(m)   = probability of each model
	std::abort();
    }
    //  else if (model_rep.get_value<string>() == "INV")
    //  (a) either specify the FREQUENCIES of the model, or
    //  (b) split every model and make a zero-scaled version of it.

    else if (model_rep.get_value<string>() == "DP") 
    {
	expression_ref base = coerce_to_RA(L, model_rep.get_child("submodel"),a,frequencies);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("dp_model"), base, n});
    }
    /*
      else if (model_rep.get_value<string>() == "Modulated")
      {
      formula_expression_ref MM = coerce_to_MM(L, model_rep.get_child("submodel"),a,frequencies);

      //    int n = ... n_base_models();
      //    return Modulated_Markov_E(MM, SimpleExchangeModel(n));
      }
      else if (model_rep.get_value<string>() == "Mixture")
      {
      if (model_args.size() < 4)
      throw myexception()<<"Mixture[..] requires at least two sub-models!";
      if (model_args[1] != "")
      throw myexception()<<"Mixture[..] does not take a submodel (i.e. with '+')";

      vector <formula_expression_ref> models;
      for(int i=0;i<model_args.size()-2;i++)
      models.push_back( coerce_to_MM(L, model_args[i+2], a, frequencies) );

      return Mixture_Model(models);
      }
      else if (model_rep.get_value<string>() == "M2") 
      {
      formula_expression_ref p1 = def_parameter("M2.fAaINV", 1.0/3, between(0,1));
      formula_expression_ref p2 = def_parameter("M2.fNeutral", 1.0/3, between(0,1));
      formula_expression_ref p3 = def_parameter("M2.fSelected", 1.0/3, between(0,1));
      formula_expression_ref m2_omega = def_parameter("M2.omega", 1.0, lower_bound(0));
      formula_expression_ref D = (identifier("DiscreteDistribution"), Tuple(p1,0.0)&
      Tuple(p2,1.0)&
      Tuple(p3,m2_omega)&
      ListEnd
      );

      D.add_expression( constructor(":~",2) + (p1&(p2&(p3&ListEnd))).exp() + (identifier("dirichlet"), List(1.0, 98.0, 1.0)) );
      D.add_expression( constructor(":~",2) + m2_omega.exp() + (identifier("logExponential"), 0.05) );

      formula_expression_ref M0 = get_M0_omega_function(L, a,frequencies,model_rep,2);

      return (identifier("multiParameter"),M0,D);
      }
      else if (model_rep.get_value<string>() == "M3u") // M3u[n,S,F]
      {
      int n = convertTo<int>(model_args[1]);

      formula_expression_ref D = ListEnd;
      formula_expression_ref F = ListEnd;
      for(int i=n-1;i>=0;i--)
      {
      string pname_f = "M3.f" + convertToString(i+1);
      string pname_w = "M3.omega" + convertToString(i+1);
      formula_expression_ref f = def_parameter("M3.f"     + convertToString(i+1), 1.0/n, between(0,1));
      formula_expression_ref w = def_parameter("M3.omega" + convertToString(i+1), 1.0, lower_bound(0), (identifier("uniform"), 0.0, 1.0));

      D = Tuple(f,w)&D;
      F = f&F;
      }
      D = (identifier("DiscreteDistribution"), D);
      D.add_expression( constructor(":~",2) + F.exp() + (identifier("dirichlet'"), n, 4.0));

      formula_expression_ref M0 = get_M0_omega_function(L,a,frequencies,model_rep,2);

      return (identifier("multiParameter"), M0, D);
      }
    */
    else if (model_rep.get_value<string>() == "M1a") // M2a[S,F]
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

	expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

	return model_expression({identifier("m1a_model"),a,S,R});
    }
    else if (model_rep.get_value<string>() == "M2a") // M2a[S,F]
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

	expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

	return model_expression({identifier("m2a_model"),a,S,R});
    }
    else if (model_rep.get_value<string>() == "M2a_Test") // M2a[S,F]
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

	expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

	return model_expression({identifier("m2a_test_model"),a,S,R});
    }
    else if (model_rep.get_value<string>() == "M3") // M[n,S,F]
    {
	int n = model_rep.get<int>("n");

	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("m3_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M3_Test") // M3_Test[n,S,F]
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("m3_test_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M7")
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("m7_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M8")
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("m8_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M8a")
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("m8a_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M8b")
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("m8b_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M8a_Test") // M8b[n,S,F]
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("m8a_test_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "branch-site")  // branch-site-test[n,S,F]
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("branch_site_test_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "dp-omega")  // branch-site-test[n,S,F]
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_rep.get_child("nuc_model"), const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_rep.get_child("freq_model"), a, frequencies);

    return model_expression({identifier("dp_omega_model"),a,n,S,R});
  }

  return {};
}

expression_ref 
get_smodel_(const module_loader& L, const ptree& model_rep, const object_ptr<const alphabet>& a,const shared_ptr<const valarray<double> >& frequencies) 
{
  if (model_rep.empty() and model_rep.data().empty())
    throw myexception()<<"Can't construct substitution model from empty description!";

  //  std::cout<<"smodel = "<<smodel<<std::endl;
  //  auto result = parse(smodel);
  //  std::cout<<result.get_value<string>()<<"\n";
  //  write_info(std::cout, result);
  //  std::cout<<std::endl;
  //  ptree model_rep = parse(smodel);

  expression_ref m;

  m = process_stack_Markov(L, model_rep, a);
  if (m) return m;

  m = process_stack_Frequencies(L, model_rep, a, frequencies);
  if (m) return m;

  m = process_stack_Multi(L, model_rep, a, frequencies);
  if (m) return m;

  throw myexception()<<"Couldn't process substitution model description \""<<show(model_rep)<<"\"";
}

expression_ref
get_smodel_(const module_loader& L,const ptree& model_rep,const object_ptr<const alphabet>& a)
{
  return get_smodel_(L, model_rep, a, shared_ptr<const valarray<double> >());
}

expression_ref
get_smodel_(const module_loader& L,const ptree& model_rep)
{
  return get_smodel_(L, model_rep, object_ptr<const alphabet>(),shared_ptr<const valarray<double> >());
}


//FIXME change to return a (model, standardized name) pair.


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param smodel_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
expression_ref
get_smodel(const module_loader& L, const ptree& model_rep, const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >& frequencies) 
{
  assert(frequencies->size() == a->size());

  // --------- Convert smodel to MultiMixtureModel ------------//
  expression_ref full_smodel = coerce_to_MMM(L, model_rep,a,frequencies);

  std::cerr<<"smodel = "<<full_smodel<<"\n";

  return full_smodel;
}

expression_ref
get_smodel(const module_loader& L, const string& smodel, const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >& frequencies) 
{
  return get_smodel(L, parse(smodel), a, frequencies);
}

/// \brief Construct a substitution::MultiModel model for a collection of alignments
///
/// \param smodel_name The name of the substitution model.
/// \param A The alignments.
///
/// This routine constructs the initial frequencies based on all of the alignments.
///
expression_ref get_smodel(const module_loader& L, const variables_map& args, const string& smodel_name,const vector<alignment>& A) 
{
  for(int i=1;i<A.size();i++)
    if (A[i].get_alphabet() != A[0].get_alphabet())
      throw myexception()<<"alignments in partition don't all have the same alphabet!";

  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(L, smodel_name, const_ptr( A[0].get_alphabet() ), frequencies);
}

expression_ref get_smodel(const module_loader& L, const variables_map& args, const string& smodel_name,const alignment& A) 
{
  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(L, smodel_name, const_ptr( A.get_alphabet() ), frequencies);
}

expression_ref get_smodel(const module_loader& L,const variables_map& args, const alignment& A) 
{
  string smodel_name = args["smodel"].as<string>();

  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(L, smodel_name, const_ptr( A.get_alphabet() ), frequencies);
}
