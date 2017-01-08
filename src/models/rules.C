#include <vector>
#include "rules.H"
#include "myexception.H"
#include "util.H"

using std::vector;
using std::string;
using boost::optional;
using boost::property_tree::ptree;

const vector< vector<vector<string>> > all_default_arguments = 
{
    {{"log","Double","N"}, {"log_model","x"}, {"x","Double"}},
    {{"Sample","a"}, {"performAction","x"}, {"x","Distribution[a]"}},
    {{"Uniform","Distribution[Double]"}, {"uniform_model","low","high"}, {"low","Double"}, {"high","Double"}},
    {{"UniformInt","Distribution[Int]"}, {"uniform_int_model","low","high"}, {"low","Int"}, {"high","Int"}},
    {{"Normal","Distribution[Double]"}, {"normal_model","mu","sigma"}, {"mu","Double"}, {"sigma","Double"}},
    {{"logNormal","Distribution[Double]"}, {"logNormal_model","lmu","lsigma"}, {"lmu","Double"}, {"lsigma","Double"}},
    {{"logLaplace","Distribution[Double]"}, {"logLaplace_model","lm","ls"}, {"lm","Double"}, {"ls","Double"}},
    {{"Laplace","Distribution[Double]"}, {"laplace_model","m","s"}, {"m","Double"}, {"s","Double"}},
    {{"logGamma","Distribution[Double]"}, {"logGamma_model","a","b"}, {"a","Double"}, {"b","Double"}},
    {{"Beta","Distribution[Double]"}, {"beta_model","a","b"}, {"a","Double"}, {"b","Double"}},
    {{"Exponential","Distribution[Double]"}, {"exponential_model","mean"}, {"mean","Double"}},
    {{"Gamma","Distribution[Double]"}, {"Distributions.gamma_model","a","b"}, {"a","Double"}, {"b","Double"}},
    {{"iid","Distribution[List[a]]"}, {"iid_model","n","dist"}, {"n","Int"}, {"dist","Distribution[a]"}},
    {{"Dirichlet","Distribution[List[Double]]"}, {"dirichlet'_model","n","x"}, {"n","Int"}, {"x","Double"}},
//    {{"Dirichlet","List[Double]"}, {"dirichlet_model","ps"}, {"ps","List[Double]"}},
    {{"Geometric","Distribution[Int]"}, {"geometric_model","p"}, {"p","Double"}},
    {{"EQU","EM[a]"}, {"equ_model"}},
    {{"JC","RA[a]"}, {"jc_model"}},
    {{"F81"}, {}, {"alphabet","Alphabet"}},
    {{"HKY","EM[a]"}, {"hky_model","kappa"}, {"kappa","Double","~logNormal[log[2],0.25]"}, },
    {{"TN","EM[a]"}, {"tn_model","kappaPur","kappaPyr"}, {"kappaPur","Double","~logNormal[log[2],0.25]"}, {"kappaPyr","Double","~logNormal[log[2],0.25]"}},
    {{"GTR","EM[a]"}, {"gtr_model","S"}, {"S","E","exchange_prior"}},
    {{"exchange_prior","E"}, {"exchange_model"}},
    {{"E","E","P"}, {"constant_exchange_model"},{"*","Double"}},
    {{"x3","EM[a]"}, {"x3_model","submodel"}, {"submodel","EM[b]"}},
    {{"PAM","EM[AA]"}, {"SModel.pam_model"}},
    {{"JTT","EM[AA]"}, {"SModel.jtt_model"}},
    {{"WAG","EM[AA]"}, {"SModel.wag_model"}},
    {{"LG","EM[AA]"}, {"SModel.lg_model"}},
    {{"Empirical","EM[a]"}, {"SModel.empirical_model","filename"}, {"filename","String"}},
    {{"M0","EM[Codon[a,b]]"}, {"m0_model","submodel","omega"}, {"submodel","EM[a]","HKY"}, {"omega","Double","~Uniform[0,1]"}},
    {{"fMutSel","RA[Codon[a,b]]"}, {"fMutSel_model","submodel","omega","ws"},
     {"omega","Double","~Uniform[0,1]"}, {"ws","List[Double]","~iid[61,logNormal[0,0.5]]"}, {"submodel","RA[a]","HKY"}},
    {{"fMutSel0","RA[Codon[a,b]]"}, {"fMutSel0_model","submodel","omega","ws"},
     {"omega","Double","~Uniform[0,1]"}, {"ws","List[Double]","~iid[20,logNormal[0,0.5]]"}, {"submodel","RA[a]","HKY"}},
// fraction ~ dirichlet' n (1 + n/2), rates ~ dirichlet' n 2
    {{"DP","MM[a]"}, {"dp_model","submodel","n","rates","frequencies"}, {"n","Int"}, {"rates","List[Double]"}, {"frequencies","List[Double]"}, {"submodel","RA[a]"}},
    {{"MultiRate","MM[a]"}, {"multiRateModel","submodel","dist","n_bins"}, {"dist","Distribution[Double]"}, {"n_bins","Int","4"}, {"submodel","RA[a]"}},
    {{"GammaRates","MM[a]"}, {"SModel.gamma_model","submodel","alpha","n"}, {"n","Int","4"}, {"alpha","Double","~logLaplace[-6,2]"}, {"submodel","RA[a]"}},
    {{"GammaInvRates","MM[a]"}, {"SModel.gamma_inv_model","submodel","alpha","pInv","n"}, {"n","Int","4"}, {"alpha","Double","~logLaplace[-6,2]"}, {"pInv","Double","~Uniform[0,1]"}, {"submodel","RA[a]"}},
    {{"log-normal","MM[a]"}, {"log_normal_model","submodel","sigmaOverMu","n"}, {"n","Int","4"}, {"sigmaOverMu","Double","~logLaplace[-3,1]"}, {"submodel","RA[a]"}},
    {{"log-normal_inv","MM[a]"}, {"log_normal_inv_model","submodel","sigmaOverMu","pInv", "n"}, {"n","Int","4"}, {"sigmaOverMu","Double","~logLaplace[-3,1]"}, {"pInv","Double","~Uniform[0,1]"}, {"submodel","RA[a]"}},
    {{"M1a","MM[Codon[a,b]]"}, {"m1a_model","nuc_model","freq_model","omega1","p1"},
     {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}, {"omega1","Double","~Uniform[0,1]"}, {"p1","Double","~Uniform[0,1]"} },
    {{"M2a","MM[Codon[a,b]]"}, {}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    {{"M2a_Test","MM[Codon[a,b]]"}, {}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    //    {{"M3u"}, {"3"}, {"nuc_model",""HKY"}, {"freq_model","F61"}},
    {{"M3","MM[Codon[a,b]]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    {{"M3_Test","MM[Codon[a,b]]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    {{"M7","MM[Codon[a,b]]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    {{"M8","MM[Codon[a,b]]"},
     {"m8_model","nuc_model","freq_model","posP","posW","n"},
     {"n","Int","4"},
     {"nuc_model","EM[a]","HKY"},
     {"freq_model","FM[Codon[a,b]]","F61"},
     {"posP","Double","~Beta[1,10]"},
     {"posW","Double","~logGamma[4,0.25]"} },
    {{"M8a","MM[Codon[a,b]]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    {{"M8a_Test","MM[Codon[a,b]]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    {{"branch-site","MM[Codon[a,b]]"}, {}, {"n","Int","2"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    {{"dp_omega","MM[Codon[a,b]]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[Codon[a,b]]","F61"}},
    {{"frequencies_prior","F"}, {"frequencies_model"}},
    {{"Freq","F","P"}, {"constant_frequencies_model"},{"*","Double"}},
    {{"F","FM[a]"}, {"plus_f_model","pi"},{"pi","F","frequencies_prior"}},
    {{"F61","FM[Codon[a,b]]"}, {"plus_f_model","pi"}, {"pi","F","frequencies_prior"}},
    {{"gwF","FM[a]"}, {"plus_gwf_model","pi","f"},{"pi","F","frequencies_prior"},{"f","Double","~Uniform[0,1]"}},
    // How about a generic frequency model that is equivalent to fMutSel0?  Can we do that?
    // Or maybe we need to do fMutSel & fMutSel0 version of the site models.
    //These should really be Triplet models, not Codon models - so we need inheritance
    {{"F1x4","FM[Codon[a,b]]"}, {"f1x4_model","pi"}, {"pi","F","frequencies_prior"}},
    {{"F3x4","FM[Codon[a,b]]"}, {"f3x4_model","pi1","pi2","pi3"}, {"pi1","F","frequencies_prior"}, {"pi2","F","frequencies_prior"}, {"pi3","F","frequencies_prior"}},
    {{"MG94","FM[Codon[a,b]]"}, {"mg94_model","pi"}, {"pi","F","frequencies_prior"}},
    {{"MG94w9","FM[Codon[a,b]]"}, {"mg94w9_model","pi1","pi2","pi3"}, {"pi1","F","frequencies_prior"}, {"pi2","F","frequencies_prior"}, {"pi3","F","frequencies_prior"}},
    {{"DNA","Alphabet","N"}, {"dna"}},
    {{"RNA","Alphabet"}, {"rna"}},
//    {{"C10","MM[a]"}, {}},
//    {{"C20","MM[a]"}, {}},
    {{"AA","Alphabet"}, {"aa"}},
// We can't write Codons[a,b] yet because we don't any mechanism for dealing with inheritance
    {{"Codons","Alphabet"}, {"codons","nuc","aa"}, {"nuc","Alphabet"}, {"aa","Alphabet","AA"}},
    {{"RCTMC","RA[a]","N"}, {"reversible_markov_model","Q","R"}, {"Q","EM[a]"}, {"R","FM[a]"}},
    {{"UnitMixture","MM[a]","N"}, {"unit_mixture_model","submodel"}, {"submodel","RA[a]"}},
    {{"MMM","MMM[a]","N"}, {"mmm_model","submodel"}, {"submodel","MM[a]"}},
    {{"RS05","IM"}, {"rs05_model","logDelta","meanIndelLengthMinus1","tau"},
     {"logDelta","Double","~Laplace[-4,0.707]"},
     {"meanIndelLengthMinus1","Double","~Exponential[10]"},
     {"tau","Double","0.001"}
    },
    {{"RS07","IM"}, {"rs07_model","logLambda","meanIndelLengthMinus1"},
     {"logLambda","Double","~Laplace[-4,0.707]"},
     {"meanIndelLengthMinus1","Double","~Exponential[10]"}
    },
    {{"RS07RelaxedRates","IM"}, {"rs07_relaxed_rates_model"}}
};

ptree parse(const string& s);
ptree parse_type(const string& s);

ptree convert_rule(const vector<vector<string>>& s)
{
    ptree rule;
    rule.put("name",s[0][0]);

    rule.push_back({"result_type",parse_type(s[0][1])});
    if (s[0].size() > 2 and s[0][2] == "P")
	rule.put("pass_arguments","true");
    if (s[1].size())
    {
	ptree call;
	for(const auto& word: s[1])
	    call.push_back({"",ptree(word)});
	rule.push_back({"call",call});
    }

    ptree args;
    for(int i=2;i<s.size();i++)
    {
	ptree arg;

	string arg_name = s[i][0];
	arg.put("arg_name",arg_name);
	arg.push_back({"arg_type",parse_type(s[i][1])});
	if (s[i].size() > 2)
	    arg.push_back({"default_value",parse(s[i][2])});
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

