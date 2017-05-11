#include <vector>
#include "rules.H"
#include "myexception.H"
#include "util.H"

using std::vector;
using std::string;
using boost::optional;
using boost::property_tree::ptree;

// TODO: implement +INV by averaging the frequencies of other models.
// TODO: print out parameter lines using the table logger.
// TODO: show tree & scale priors in bp-analyze output.
// TODO: change scale_means_only -> scale_all_branches
// TODO: *allow referring to other parameters in parameter values (default values and otherwise)
// TODO: remove triggers
// TODO: move stuff (e.g. logging) out of models
// TODO: clean up loggers.{H,C} to use lambda functions
// TODO: clean up transition kernels to use lambda functions?
// TODO: find some way to run under the prior?
// TODO: rewrite frequencies_prior..
// Q: fmutsel version of m3, etc.
// Q: how to share GTR between genes in gtr+gwf?

const vector< vector<vector<string>> > all_default_arguments = 
{
    {{"log", "Double", "GN"}, {"Prelude.log[x]"}, {"x", "Double"}},
    {{"exp", "Double", "GN"}, {"Prelude.exp[x]"}, {"x", "Double"}},
    {{"Sample", "a"}, {"Prelude.performAction[x]"}, {"x", "Distribution[a]"}},

    {{"Uniform", "Distribution[Double]", "G"}, {"Distributions.uniform[low,high]"}, {"low", "Double"}, {"high", "Double"}},
    {{"UniformInt", "Distribution[Int]", "G"}, {"Distributions.uniform_int[low,high]"}, {"low", "Int"}, {"high", "Int"}},
    {{"Normal", "Distribution[Double]", "G"}, {"Distributions.normal[mu,sigma]"}, {"mu", "Double"}, {"sigma", "Double"}},
    {{"logNormal", "Distribution[Double]", "G"}, {"Distributions.logNormal[lmu,lsigma]"}, {"lmu", "Double"}, {"lsigma", "Double"}},
    {{"logLaplace", "Distribution[Double]", "G"}, {"Distributions.logLaplace[lm,ls]"}, {"lm", "Double"}, {"ls", "Double"}},
    {{"Laplace", "Distribution[Double]", "G"}, {"Distributions.laplace[m,s]"}, {"m", "Double"}, {"s", "Double"}},
    {{"logGamma", "Distribution[Double]", "G"}, {"Distributions.logGamma[a,b]"}, {"a", "Double"}, {"b", "Double"}},
    {{"Beta", "Distribution[Double]", "G"}, {"Distributions.beta[a,b]"}, {"a", "Double"}, {"b", "Double"}},
    {{"Exponential", "Distribution[Double]", "G"}, {"Distributions.exponential[mean]"}, {"mean", "Double"}},
    {{"Gamma", "Distribution[Double]", "G"}, {"Distributions.gamma[a,b]"}, {"a", "Double"}, {"b", "Double"}},

    {{"Bernoulli", "Distribution[Int]", "G"}, {"Distributions.bernoulli[p]"}, {"p", "Double"}},
    {{"Binomial", "Distribution[Int]", "G"}, {"Distributions.binomial[n,p]"}, {"n", "Int"}, {"p", "Double"}},
    {{"Geometric", "Distribution[Int]", "G"}, {"Distributions.geometric[p]"}, {"p", "Double"}},

    {{"iid", "Distribution[List[a]]", "G"}, {"Distributions.iid[n,dist]"}, {"n", "Int"}, {"dist", "Distribution[a]"}},
    {{"Dirichlet", "Distribution[List[Double]]", "G"}, {"Distributions.dirichlet'[n,x]"}, {"n", "Int"}, {"x", "Double"}},

//  We need a way to construct lists, not from a distribution.
    {{"List", "List[a]", "L"},
     {"Prelude.sequence"},
     {"*", "a"}},

    {{"Pair", "Pair[a,b]", "G"},{"(,)[first,second]"},{"first", "a"}, {"second", "b"}},

    {{"EQU", "EM[a]", "G"}, {"SModel.equ[A]"}, {"A", "a", "LAMBDA"}},

    {{"JC", "RA[a]", "G"},
     {"SModel.jukes_cantor[A]"},
     {"A", "a", "LAMBDA"}},

    {{"K80", "RA[a]", "G"},
     {"SModel.k80[kappa,A]"},
     {"kappa", "Double", "~logNormal[log[2],0.25]"},
     {"A", "a", "LAMBDA"} },

    {{"F81", "RA[a]", "G"},
     {"SModel.f81[pi,A]"},
     {"pi", "F", "frequencies_prior", "A"},
     {"A", "a", "LAMBDA"} },

    {{"HKY", "EM[a]", "G", "Nucleotides[a]"},
     {"SModel.hky[kappa,alphabet]"},
     {"kappa", "Double", "~logNormal[log[2],0.25]"},
     {"alphabet", "a", "LAMBDA"}},

    {{"TN", "EM[a]", "G"},
     {"SModel.tn[kappaPur,kappaPyr,A]"},
     {"kappaPur", "Double", "~logNormal[log[2],0.25]"},
     {"kappaPyr", "Double", "~logNormal[log[2],0.25]"},
     {"A", "a", "LAMBDA"}},

    {{"GTR", "EM[a]", "G"},
     {"SModel.gtr[S,A]"},
     {"S", "E", "exchange_prior", "A"},
     {"A", "a", "LAMBDA"}},

    {{"exchange_prior", "E"}, {"SModel.exchange_model"}},

    {{"E", "E", "P"}, {"SModel.constant_exchange_model"},{"*", "Double"}},

    {{"x3", "EM[a]","G"},
     {"SModel.singlet_to_triplet_exchange[A,submodel]"},
     {"submodel", "EM[b]","","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"PAM", "EM[AA]", "G"}, {"SModel.pam[A]"}, {"A", "a", "LAMBDA"}},

    {{"JTT", "EM[AA]", "G"}, {"SModel.jtt[A]"}, {"A", "a", "LAMBDA"}},

    {{"WAG", "EM[AA]", "G"}, {"SModel.wag[A]"}, {"A", "a", "LAMBDA"}},

    {{"LG", "EM[AA]", "G"}, {"SModel.lg[A]"}, {"A", "a", "LAMBDA"}},

    {{"Empirical", "EM[a]", "G"},
     {"SModel.empirical[A,filename]"},
     {"filename", "String"},
     {"A","a","LAMBDA"}},

    {{"M0", "EM[Codon[a,b]]", "G", "Nucleotides[a],AminoAcids[b]"},
     {"SModel.m0[A,submodel,omega]"},
     {"omega", "Double", "~Uniform[0,1]"},
     {"submodel", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"fMutSel", "RA[Codon[a,b]]",},
     {"SModel.fMutSel_model[submodel,omega,ws]"},
     {"omega", "Double", "~Uniform[0,1]"},
     // ws: We are setting the sampling rate to 1/sqrt(n_letters) on these.
     // ws: We are logging these inside the function.
     {"ws", "List[Double]", "~iid[61,logNormal[0,0.5]]"},
     {"submodel", "RA[a]", "HKY"}},

    {{"fMutSel0", "RA[Codon[a,b]]"},
     {"SModel.fMutSel0_model[submodel,omega,ws]"},
     {"omega", "Double", "~Uniform[0,1]"},
     {"ws", "List[Double]", "~iid[20,logNormal[0,0.5]]"},
     {"submodel", "RA[a]", "HKY"}},

    // fraction ~ dirichlet' n (1 + n/2), rates ~ dirichlet' n 2
    {{"DP", "MM[a]","G"},
     {"SModel.dp[submodel,rates,frequencies,A]"},
     {"rates", "List[Double]", "~Dirichlet[n,2]"},
     {"frequencies", "List[Double]", "~Dirichlet[n,3]"},
     {"n","Int","4"},
     {"submodel", "RA[a]","","A"},
     {"A","a","LAMBDA"}},

    {{"MultiRate", "MM[a]", "G"},
     {"SModel.multi_rate_unif_bins[submodel,dist,n_bins]"},
     {"dist", "Distribution[Double]"},
     {"n_bins", "Int", "4"},
     {"submodel", "RA[a]","","A"},
     {"A","a","LAMBDA"}},

    {{"GammaRates", "MM[a]", "G"},
     {"SModel.gamma_rates[submodel,alpha,n]"},
     {"n", "Int", "4"},
     {"alpha", "Double", "~logLaplace[-6,2]"},
     {"submodel", "RA[a]", "", "A"},
     {"A", "a", "LAMBDA"}},

    {{"GammaInvRates", "MM[a]", "G"},
     {"SModel.gamma_inv_rates[submodel,alpha,pInv,n]"},
     {"n", "Int", "4"},
     {"alpha", "Double", "~logLaplace[-6,2]"},
     {"pInv", "Double", "~Uniform[0,1]"},
     {"submodel", "RA[a]", "", "A"},
     {"A", "a", "LAMBDA"}},

    {{"log_normal_rates", "MM[a]", "G"},
     {"SModel.log_normal_rates[submodel,sigmaOverMu,n]"},
     {"n", "Int", "4"},
     {"sigmaOverMu", "Double", "~logLaplace[-3,1]"},
     {"submodel", "RA[a]", "", "A"},
     {"A", "a", "LAMBDA"}},
    
    {{"log_normal_inv_rates", "MM[a]", "G"},
     {"SModel.log_normal_inv_rates[submodel,sigmaOverMu,pInv,n]"},
     {"n", "Int", "4"},
     {"sigmaOverMu", "Double", "~logLaplace[-3,1]"},
     {"pInv", "Double", "~Uniform[0,1]"},
     {"submodel", "RA[a]", "", "A"},
     {"A", "a", "LAMBDA"}},

    {{"M1a", "MM[Codon[a,b]]","G"},
     {"SModel.m1a[nuc_model,freq_model,omega1,p1,A]"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61","A"},
     {"omega1", "Double", "~Uniform[0,1]"},
     {"p1", "Double", "~Uniform[0,1]"},
     {"A","a","LAMBDA"}},
    
    {{"M2a", "MM[Codon[a,b]]","G"},
     {"SModel.m2a[nuc_model,freq_model,omega1,p1,posP,posW,A]"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61","A"},
     {"omega1", "Double", "~Uniform[0,1]"},
     {"p1", "Double", "~Uniform[0,1]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"A","a","LAMBDA"}},

    {{"M2a_Test", "MM[Codon[a,b]]", "G"},
     {"SModel.m2a_test[nuc_model,freq_model,omega1,p1,posP,posW,posSelection,A]"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61","A"},
     {"omega1", "Double", "~Uniform[0,1]"},
     {"p1", "Double", "~Uniform[0,1]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"posSelection", "Int", "~Bernoulli[0.5]"},
     {"A","a","LAMBDA"}},

    //    {{"M3u"}, {"3"}, {"nuc_model", ""HKY"}, {"freq_model", "F61"}},
    //  Maybe we should introduce a way to sample Dirichlet and IIDs of the same length and then zip them?
    //   * would this solve some of our woes with the DP model?
    //   * this does NOT solve the issue of the dirichlet weight depending on n

    {{"M3", "MM[Codon[a,b]]", "G"},
     {"SModel.m3[nuc_model,freq_model,ps,omegas,A]"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61", "A"},
     {"ps", "List[Double]", "~Dirichlet[n,2]"},
     {"omegas", "List[Double]", "~iid[n,Uniform[0,1]]"},
     {"n","Int","4"},
     {"A", "a", "LAMBDA"}},

    {{"M3_Test", "MM[Codon[a,b]]", "G"},
     {"SModel.m3_test[nuc_model,freq_model,ps,omegas,posP,posW,posSelection,A]"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61", "A"},
     {"ps", "List[Double]", "~Dirichlet[n,2]"},
     {"omegas", "List[Double]", "~iid[n,Uniform[0,1]]" },
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"posSelection", "Int", "~Bernoulli[0.5]"},
     {"n","Int","4"},
     {"A", "a", "LAMBDA"}},

    {{"M7", "MM[Codon[a,b]]", "G"},
     {"SModel.m7[nuc_model,freq_model,mu,gamma,n,A]"},
     {"n", "Int", "4"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"gamma", "Double", "~Beta[1,10]"},
     {"A", "a", "LAMBDA"}},
    
    {{"M8", "MM[Codon[a,b]]","G"},
     {"SModel.m8[nuc_model,freq_model,mu,gamma,n,posP,posW,A]"},
     {"n", "Int", "4"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"gamma", "Double", "~Beta[1,10]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"A","a","LAMBDA"}},

    {{"M8a", "MM[Codon[a,b]]","G"},
     {"SModel.m8a[nuc_model,freq_model,mu,gamma,n,posP,A]"},
     {"n", "Int", "4"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"gamma", "Double", "~Beta[1,10]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"A","a","LAMBDA"}},
    
    {{"M8a_Test", "MM[Codon[a,b]]","G"},
     {"SModel.m8a_test[nuc_model,freq_model,mu,gamma,n,posP,posW,posSelection,A]"},
     {"n", "Int", "4"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"gamma", "Double", "~Beta[1,10]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"posSelection", "Int", "~Bernoulli[0.5]"},
     {"A","a","LAMBDA"}},

    {{"branch-site", "MMM[Codon[a,b]]", "G"},
     {"SModel.branch_site_test[nuc_model,freq_model,fs,omegas,posP,posW,posSelection,A]"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61", "A"},
     {"fs", "List[Double]", "~Dirichlet[2,1]"},
     {"omegas", "List[Double]", "~iid[1,Uniform[0,1]]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"posSelection", "Int", "~Bernoulli[0.5]"},
     {"A","a","LAMBDA"}},

    {{"dp_omega", "MM[Codon[a,b]]", "G"},
     {"SModel.dp_omega[nuc_model,freq_model,mu,omegas,A]"},
     {"nuc_model", "EM[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FM[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"omegas", "List[Double]", "~Dirichlet[n,1]"},
     {"n","Int","4"},
     {"A","a","LAMBDA"}},
    
    {{"frequencies_prior", "F"}, {"SModel.frequencies_model"}},
    
    {{"Freq", "F", "P"}, {"SModel.constant_frequencies_model"},{"*", "Double"}},
    
    {{"Freq2", "F"}, {"SModel.constant_frequencies_model2[dict]"},{"dict", "List[Pair[String,Double]]"}},
    
    {{"F", "FM[a]", "G"},
     {"SModel.plus_f[A,pi]"},
     {"pi", "F", "frequencies_prior", "A"},
     {"A", "a", "LAMBDA"}},
    
    {{"F61", "FM[Codon[a,b]]", "G"}, {"SModel.plus_f[A,pi]"},{"pi", "F", "frequencies_prior", "A"}, {"A", "a", "LAMBDA"}},

    {{"gwF", "FM[a]", "G"}, {"SModel.plus_gwf[A,pi,f]"},{"pi", "F", "frequencies_prior", "A"},{"f", "Double", "~Uniform[0,1]"},{"A", "a", "LAMBDA"}},
    // How about a generic frequency model that is equivalent to fMutSel0?  Can we do that?
    // Or maybe we need to do fMutSel & fMutSel0 version of the site models.
    //These should really be Triplet models, not Codon models - so we need inheritance

    {{"F1x4", "FM[Codon[a,b]]", "G"},
     {"SModel.f1x4[A,pi]"},
     {"pi", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"F3x4", "FM[Codon[a,b]]", "G"},
     {"SModel.f3x4[A,pi1,pi2,pi3]"},
     {"pi1", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"pi2", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"pi3", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"MG94", "FM[Codon[a,b]]", "G"},
     {"SModel.mg94[pi,A]"},
     {"pi", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"MG94w9", "FM[Codon[a,b]]", "G"},
     {"SModel.mg94w9[pi1,pi2,pi3,A]"},
     {"pi1", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"pi2", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"pi3", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"DNA", "Alphabet", "N"}, {"dna"}},
    {{"RNA", "Alphabet"}, {"rna"}},
//    {{"C10", "MM[a]"}, {}},
//    {{"C20", "MM[a]"}, {}},
    {{"AA", "Alphabet"}, {"aa"}},
// We can't write Codons[a,b] yet because we don't any mechanism for dealing with inheritance
    {{"Codons", "Alphabet"}, {"codons[nuc,aa]"}, {"nuc", "Alphabet"}, {"aa", "Alphabet", "AA"}},
    {{"RCTMC", "RA[a]", "GN"},
     {"SModel.reversible_markov[Q,R]"},
     {"Q", "EM[a]", "", "A"},
     {"R", "FM[a]", "", "A"},
     {"A", "a", "LAMBDA"}},

    {{"UnitMixture", "MM[a]", "NG"},
     {"SModel.unit_mixture[submodel]"},
     {"submodel", "RA[a]", "", "A"},
     {"A", "a", "LAMBDA"},
    },

    {{"MMM", "MMM[a]", "NG"},
     {"SModel.mmm[submodel]"},
     {"submodel", "MM[a]", "", "A"},
     {"A", "a", "LAMBDA"}
    },

    {{"RS05", "IM", "G"},
     {"IModel.rs05[logDelta,meanIndelLengthMinus1,tau,tree]"},
     {"logDelta", "Double", "~Laplace[-4,0.707]"},
     {"meanIndelLengthMinus1", "Double", "~Exponential[10]"},
     {"tau", "Double", "0.001"},
     {"tree", "tree", "LAMBDA"}
    },

    {{"RS07", "IM", "G"},
     {"IModel.rs07[logLambda,meanIndelLengthMinus1,tree]"},
     {"logLambda", "Double", "~Laplace[-4,0.707]"},
     {"meanIndelLengthMinus1", "Double", "~Exponential[10]"},
     {"tree", "tree", "LAMBDA"}
    },

    {{"RS07RelaxedRates", "IM"}, {"IModel.rs07_relaxed_rates_model"}}
};

ptree parse(const string& s);
ptree parse_type(const string& s);

ptree parse_constraints(const string& s)
{
    vector<string> ss = split(s,',');
    ptree constraints;
    for(auto& c: ss)
	constraints.push_back({"", parse_type(c)});
    return constraints;
}

ptree convert_rule(const vector<vector<string>>& s)
{
    ptree rule;
    rule.put("name",s[0][0]);

    rule.push_back({"result_type",parse_type(s[0][1])});
    string attributes = (s[0].size() > 2)?s[0][2]:"";

    if (contains_char(attributes, 'P')) rule.put("pass_arguments", "true");
    if (contains_char(attributes, 'L')) rule.put("list_arguments", "true");
    if (contains_char(attributes, 'G')) rule.put("generate_function", "true");
    if (contains_char(attributes, 'N')) rule.put("no_log", "true");

    if (s[0].size() >= 4)
	rule.push_back({"Constraints", parse_constraints(s[0][3])});

    // Add the call
    rule.push_back({"call", parse_type(s[1][0])});
    assert(s[1].size() == 1);

    ptree args;
    for(int i=2;i<s.size();i++)
    {
	ptree arg;

	string arg_name = s[i][0];
	arg.put("arg_name",arg_name);
	arg.push_back({"arg_type",parse_type(s[i][1])});
	if (s[i].size() > 2 and s[i][2] == "LAMBDA")
	    arg.put("no_apply", "true"); // FIXME -- this only makes sense for the last few args
	else if (s[i].size() > 2 and not s[i][2].empty())
	    arg.push_back({"default_value",parse(s[i][2])});

	if (s[i].size() > 3 and not s[i][3].empty())
	    arg.push_back({"applied_args",parse_type(s[i][3])});

	args.push_back({"",arg});
    }
    rule.push_back({"args",args});
//    write_info(std::cout, rule);
    return rule;
}

optional<Rule> get_rule_for_func(const string& s)
{
    for(const auto& rule: all_default_arguments)
	if (rule[0][0] == s)
	    return convert_rule(rule);
    return boost::none;
}

Rule require_rule_for_func(const string& s)
{
    if (auto rule = get_rule_for_func(s))
	return *rule;
    else
	throw myexception()<<"No function '"<<s<<"'.";
}

ptree get_arg(const Rule& rule, const string& arg_name)
{
    for(const auto& arg: rule.get_child("args"))
	if (arg.second.get<string>("arg_name") == arg_name)
	    return arg.second;
    throw myexception()<<"Rule for function '"<<rule.get<string>("name")<<"' has no argument '"<<arg_name<<"'";
}

string get_keyword_for_positional_arg(const string& head, int i)
{
    auto rule = require_rule_for_func(head);

    const auto arguments = rule.get_child("args");
    if (i > arguments.size())
	throw myexception()<<"Trying to access positional arg "<<i+1<<" for '"<<head<<"', which only has "<<arguments.size()<<" positional arguments.";

    auto it = arguments.begin();
    for(int j=0;j<i;j++)
	it++;
	
    return it->second.get<string>("arg_name");
}

ptree get_type_for_arg(const Rule& rule, const string& arg)
{
    return get_arg(rule,arg).get_child("arg_type");
}

ptree get_type_for_arg(const string& func, const string& arg)
{
    if (auto rule = get_rule_for_func(func))
	return get_type_for_arg(*rule, arg);
    else
	return ptree("?");
}

ptree get_result_type(const string& func)
{
    if (can_be_converted_to<int>(func)) return ptree("Int");
    if (can_be_converted_to<double>(func)) return ptree("Double");

    if (auto rule = get_rule_for_func(func))
	return rule->get_child("result_type");
    else
	return ptree("?");
}

ptree get_result_type(const ptree& model_rep)
{
    return get_result_type(model_rep.get_value<string>());
}

