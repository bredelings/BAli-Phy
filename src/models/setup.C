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

// 4b. Make model_expressions for sampling frequency.
// 6b. Print the expanded model expression.
// 7. Allow specifying lists?
// 8. Allow specifying priors to the frequency model.
// 10. Allow using user-specified models from the command line.
// 11. ? How to deal with things that need model-dependent argument names?
// 12. ? How to deal with things that need programmatic model specification?
// 13. ? Why can't we sort the DP rates (which would be a great use of programmatic model specification)?
//*17. Write a function to go BACK from ptrees to strings.
// 18. Allow specifying the alphabet in the smodel, instead of as a command-line option.
// 19. Allow generic parsing of functions completely from their description, instead of writing code for each one.
// 20. Allow specifying the frequencies in the expression as (e.g.) empirical frequencies, for a group of alignments.
// 21. Allow specifying and receiving help information for each function, and for its arguments.
// 22. Eliminate as many ***_model functions as possible.
// 23. Allow GTR aminoacid models.
// 24. Allow a full term-rewriting system, with unification on types.  This will allow passing
//     computing expressions before we know the alphabet that allow the alphabet to be a (type) variable.
//     We can then use these expressions to narrow the alphabets we try when reading the alignment matrix.
//
//     This should also allow propagating an integer argument to default values for other variables, although
//     this will not work for evaluated expressions -- or not very well.

// DONE
// 4a. Make model_expressions for prefixing, logging.
// 5. Associate types with each argument.
// 6a. Expand default arguments in the model expression.
// 9. Allow separately specifying random variables to e.g. indel model.
// 14. Make 'translate' more intelligent, so that it can handle top-level expressions
//     NOT being actions, but arguments BEING actions.
// 15. Specify the types of arguments and results (for coercion).
// 16. Do the coercion on the ptree or string level.

#include <vector>
#include <boost/program_options.hpp>
#include "models/setup.H"
#include "util.H"
#include "myexception.H"
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/info_parser.hpp>
#include <boost/optional.hpp>
#include <boost/property_tree/json_parser.hpp>
#include "models/rules.H"
#include "models/parse.H"
#include "models/translate.H"
#include "computation/model_expression.H"
#include "computation/operations.H"

using boost::property_tree::ptree;
using boost::optional;
using std::string;
using std::pair;
using std::set;
using std::vector;
using std::valarray;
using boost::program_options::variables_map;
using boost::shared_ptr;

// N = no logging or arguments
// M = this expression needs to be performed

/*
  Maybe convert 'rule' information to JSON like this:
  {
  name: "Uniform", 
  type: "Double", 
  action: true, 
  call: ["uniform","a","b"], 
  args: [["a","Double",0],["b","Double",1],
  }
*/

/// Split a string of the form key=value into {key,value}
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

auto index(const ptree& p, int i)
{
    if (i > p.size())
	throw myexception()<<"Can't get entry "<<i<<" for tree with size "<<p.size();
    auto it = p.begin();
    for(int j=0;j<i;j++)
	it++;
    return *it;
}

ptree array_index(const ptree& p, int i)
{
    return index(p,i).second;
}

expression_ref get_model_as(const ptree& type, const ptree& model_rep);
expression_ref get_model_as(const string& type, const ptree& model_rep)
{
    return get_model_as(parse_type(type), model_rep);
}

expression_ref process_stack_functions(const ptree& model_rep)
{
    string name = model_rep.get_value<string>();

    auto rule = get_rule_for_func(name);
    if (not rule) return {};
    if (not rule->count("call")) return {};
	
    bool pass_arguments = rule->get("pass_arguments",false) == true;
    ptree call = rule->get_child("call");
    ptree args = rule->get_child("args");
    
    expression_ref E = identifier(array_index(call,0).get_value<string>());
    if (pass_arguments)
    {
	ptree arg_type = get_type_for_arg(*rule, "*");
	vector<expression_ref> arguments;
	for(const auto& child: model_rep)
	{
	    string arg_name = child.first;
	    expression_ref arg = get_model_as(arg_type, model_rep.get_child(arg_name));
	    arguments.push_back(Tuple(arg_name,arg));
	}
	E = (E,get_list(arguments));
    }
    else
	for(int i=1;i<call.size();i++)
	{
	    string arg_name = array_index(call,i).get_value<string>();
	    ptree arg_tree = get_arg(*rule, arg_name);
	    ptree arg_type = arg_tree.get_child("arg_type");
	    expression_ref arg = get_model_as(arg_type, model_rep.get_child(arg_name));
	    E = (E,arg);
	}
    return E;

    return {};
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
///
expression_ref process_stack_Markov(const ptree& model_rep)
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

    if (model_rep.get_value<string>() == "Empirical") 
    {
	if (not model_rep.count("alphabet"))
	    throw myexception()<<"Model '"<<model_rep.get_value<string>()<<"' is missing parameters 'alphabet'";

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	return (identifier("SModel.empirical"), alphabet, model_rep.get<string>("filename"));
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
    else if (model_rep.get_value<string>() == "fMutSel0")
    {
	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref nuc_rm = get_model_as("RA[a]",model_rep.get_child("submodel"));

	return model_expression({identifier("fMutSel0_model"), alphabet , nuc_rm});
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
expression_ref process_stack_Frequencies(const ptree&)
{
    expression_ref R;

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

    return R;
}

expression_ref process_stack_Multi(const ptree& model_rep)
{
    if (model_rep.get_value<string>() == "INV") 
    {
	expression_ref base = get_model_as("RA[a]", model_rep.get_child("submodel"));

	return model_expression({identifier("inv_model"), base});
    }
    else if (model_rep.get_value<string>() == "log-normal") 
    {
	expression_ref base = get_model_as("RA[a]", model_rep.get_child("submodel"));

	int n = model_rep.get<int>("n");

	return model_expression({identifier("log_normal_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "log-normal_inv") 
    {
	expression_ref base = get_model_as("RA[a]", model_rep.get_child("submodel"));

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
	expression_ref base = get_model_as("RA[a]", model_rep.get_child("submodel"));

	int n = model_rep.get<int>("n");

	return model_expression({identifier("dp_model"), base, n});
    }
    /*
      else if (model_rep.get_value<string>() == "Modulated")
      {
      formula_expression_ref MM = coerce_to_MM(model_rep.get_child("submodel"));

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
      models.push_back( coerce_to_MM(model_args[i+2]) );

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

      formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_rep,2);

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

      formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_rep,2);

      return (identifier("multiParameter"), M0, D);
      }
    */
    else if (model_rep.get_value<string>() == "M1a") // M2a[S,F]
    {
	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m1a_model"),alphabet,S,R});
    }
    else if (model_rep.get_value<string>() == "M2a") // M2a[S,F]
    {
	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m2a_model"),alphabet,S,R});
    }
    else if (model_rep.get_value<string>() == "M2a_Test") // M2a[S,F]
    {
	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m2a_test_model"),alphabet,S,R});
    }
    else if (model_rep.get_value<string>() == "M3") // M[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m3_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M3_Test") // M3_Test[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m3_test_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M7")
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m7_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M8")
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m8_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M8a")
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m8a_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M8b")
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m8b_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M8a_Test") // M8b[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m8a_test_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "branch-site")  // branch-site-test[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));
	
	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("branch_site_test_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "dp-omega")  // branch-site-test[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_model_as("Alphabet", model_rep.get_child("alphabet"));
	
	expression_ref S = get_model_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_model_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("dp_omega_model"),alphabet,n,S,R});
    }

    return {};
}

expression_ref get_model_(const ptree& model_rep)
{
    if (model_rep.empty() and model_rep.data().empty())
	throw myexception()<<"Can't construct substitution model from empty description!";

    //  std::cout<<"model = "<<model<<std::endl;
    //  auto result = parse(model);
    //  std::cout<<result.get_value<string>()<<"\n";
    //  write_info(std::cout, result);
    //  std::cout<<std::endl;
    //  ptree model_rep = parse(model);

    // If we are processing an Int, just return an int.
    if (can_be_converted_to<int>(model_rep.get_value<string>()))
    {
	expression_ref value = model_rep.get_value<int>();
	return (identifier("return"), value);
    }

    // If we are processing a Double, just return a double
    if (can_be_converted_to<double>(model_rep.get_value<string>()))
    {
	expression_ref value = model_rep.get_value<double>();
	return (identifier("return"), value);
    }

    expression_ref m;

    m = process_stack_functions(model_rep);
    if (m) return m;

    m = process_stack_Markov(model_rep);
    if (m) return m;

    m = process_stack_Frequencies(model_rep);
    if (m) return m;

    m = process_stack_Multi(model_rep);
    if (m) return m;

    throw myexception()<<"Couldn't process substitution model description \""<<show(model_rep)<<"\"";
}

expression_ref get_model_as(const ptree& required_type, const ptree& model_rep)
{
    if (model_rep.empty() and model_rep.data().empty())
    {
	std::cout<<show(model_rep)<<std::endl;
	throw myexception()<<"Can't construct type '"<<unparse_type(required_type)<<"' from empty description!";
    }

    ptree result_type = get_result_type(model_rep);
    string name = model_rep.get_value<string>();

    if (required_type.get_value<string>() == "Double" and result_type.get_value<string>() == "Int")
    {
	double d;
	if (can_be_converted_to<double>(name, d))
	{
	    expression_ref value = d;
	    return (identifier("return"), d);
	}
    }

    if (not can_unify(result_type, required_type))
	throw myexception()<<"Expected type '"<<unparse_type(required_type)<<"' but got '"<<name<<"' of type "<<unparse_type(result_type);

    return get_model_(model_rep);
}

/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param model_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
expression_ref get_model(const string& type, const ptree& model_rep)
{
    // --------- Convert model to MultiMixtureModel ------------//
    expression_ref full_model = get_model_as(type, model_rep);

    if (log_verbose)
	std::cout<<"full_model = "<<full_model<<std::endl;

    return full_model;
}

expression_ref get_model(const string& type, const string& model)
{
//    std::cout<<"model1 = "<<model<<std::endl;

    auto p = translate_model(type, model);
    auto model_tree = p.first;
    auto equations = p.second;
    if (log_verbose)
    {
	std::cout<<"model = "<<unparse(model_tree)<<std::endl;
	std::cout<<show(equations,4)<<std::endl;
    }
    return get_model(type, model_tree);
}
