#include <vector>
#include "rules.H"
#include "myexception.H"
#include "util.H"

using std::vector;
using std::string;
using boost::optional;
using boost::property_tree::ptree;

// TODO: reject HKY+HKY -- reduce constraints.
//       reject HKY model with amino acid data.
//       reject M3[LG,F1x4]

// TODO: decrease memory usage for pairwise alignments.
//       - make pairwise alignments represent the relevant bits without translation (e.g. make convert_to_bits a no-op)
//       - make pairwise contain two dynamic_bitsets

// TODO: decrease memory usage for HMM::bitmask_t from 64 bits to 8 bits.

// TODO: try implementing a horseshoe model for f and s

// TODO: complain when setting calculator=SEV and imodel!=none

// TODO: devirtualize pool::allocate

// TODO: implement iteration-based color for 3D MDS plot.

// TODO: reduce memory:
//       - toss identifiers, and make them not heads.
//       - further reduce memory for pairwise alignments.

// TODO: move some things into a computation/machine directory
//       separate parsing and model-creation code.
//       move parsing code into computation directory

// TODO: speed up likelihood code by caching/indexing on (node,index)
//       - caching based on (node,index) means that we'd have to expose (node,index), which is odd.
//       - can we internally cache the edges_before_edge computation?
//       - how about making branch lookup constant, and changing the prev/next fields?

// QUESTION: how do we think about indexing on the root instead of just tracking the execution graph?
//           how does this relate to factorial n = case n or n' => factorial n'?

// TODO: full laziness transformation.  <- WE ARE HERE.
//       do simplification before and after full laziness
//       case-merging
//       generate un-optimized code for cases (e.g. def_function)
//       handle x@(y:ys) and guards in case statements.
//       when a depends on b, fully simplify b before trying to simplify a. (split modules into a LIST of topdecls)
//       unpack_cstring (AFTER splitting up dependences in modules)
//       allow reasonable exporting, to break up SModel.hs into submodules that are exported from it.
//       READ SANTOS THESIS.
//       Q: why is floating-inwards necessary?  This seems to substitute for more intelligent analyses...
//       get optimization examples from the thesis?
//       allow case a+x of v -> E to put v EITHER into a closure OR on a stack.

// TODO: add covarion and CAT10 and CAT20 models.

//--- Up to here, just do it.

// TODO: fix compilation with recent boost.
// TODO: change scale_means_only -> scale_all_branches

// TODO: move logging, scale_factor, prefixing, etc. out of models.

// TODO: make a function that generates a JSON object (or property tree) object in order to log things.
//       - maybe make things like frequencies be implemented as a Map [(String,Double)]
//       - maybe just make a specialized logger and/or reader that treats a [String] and a [Double] as map from String->Double.
//         * make an automatic conversion rule so we can supply e.g. {A:0.1,T:0.2,C:0.3,G:0.4} and get the numbers in the right order,
//           and also a way of logging them in the right order?
//       - can we handle e.g. {A:log[0.1],T:1.0-Log[0.1]} ?

// TODO: Add a "scope" construct to haskell to handle things that shouldn't be moved out of scope?

// TODO: make sub-partitions: split 0/1 partitions under Mk or Mkv into sub-partitions where alphabet size = character size.

// HARD TODO: missing complete genes? (or document why not).

// HARD TODO: --help implicit value?
//   Also, say which parameter and function if we have an argument type mismatch.

// TODO: * Add 01 alphabets.
//       * HARD: Allow loading 01234 character data -- probably requires sub-partitions.
//       * Condition on columns not being invariant.
//         - this only makes sense for constant alignments.
//         - perhaps each column should kind of be a separate partition.
//         - this isn't SEQUENCE data.
//         - but manuscripts ARE sequences
//         - should we handle insertions & deletions that way?
//       * Q: how to add the extra term?

// TODO: Q: Relatedly, how much slowdown does the alignment prior multiplication tree cause?

// TODO: change Scale and *T (branch lengths) into an array, and log them that way.

// TODO: rewrite tree reader/writer functions to use lambdas.

// TODO: allow addition/subtraction?
// ? TODO: clean up loggers.{H,C} to use lambda functions
// ? TODO: clean up transition kernels to use lambda functions?
// TODO: find some way to run under the prior?
// TODO: rewrite frequencies_prior..
// Q: fmutsel version of m3, etc.
// Q: how to share GTR between genes in gtr+gwf?

const vector< vector<vector<string>> > all_default_arguments = 
{
    // What we want here is Num a => a -> a -> a
    {{"Add", "a", "GN", "Num[a]"}, {"Prelude.+[x,y]"}, {"x", "a"}, {"y", "a"}},
    {{"Sub", "a", "GN", "Num[a]"}, {"Prelude.-[x,y]"}, {"x", "a"}, {"y", "a"}},
    {{"Mul", "a", "GN", "Num[a]"}, {"Prelude.*[x,y]"}, {"x", "a"}, {"y", "a"}},
    {{"Div", "a", "GN", "Num[a]"}, {"Prelude./[x,y]"}, {"x", "a"}, {"y", "a"}},
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

    {{"EQU", "ExchangeModel[a]", "G"}, {"SModel.equ[A]"}, {"A", "a", "LAMBDA"}},

    {{"JC", "RA[a]", "G"},
     {"SModel.jukes_cantor[A]"},
     {"A", "a", "LAMBDA"}},

    {{"K80", "RA[a]", "G", "Nucleotides[a]"},
     {"SModel.k80[kappa,A]"},
     {"kappa", "Double", "~logNormal[log[2],0.25]"},
     {"A", "a", "LAMBDA"} },

    {{"F81", "RA[a]", "G"},
     {"SModel.f81[pi,A]"},
     {"pi", "F", "frequencies_prior", "A"},
     {"A", "a", "LAMBDA"} },

    {{"HKY", "ExchangeModel[a]", "G", "Nucleotides[a]"},
     {"SModel.hky[kappa,alphabet]"},
     {"kappa", "Double", "~logNormal[log[2],0.25]","","Transition/transversion ratio"},
     {"alphabet", "a", "LAMBDA"}},

    {{"TN", "ExchangeModel[a]", "G", "Nucleotides[a]"},
     {"SModel.tn[kappaPur,kappaPyr,A]"},
     {"kappaPur", "Double", "~logNormal[log[2],0.25]","","A<->G Transition/transversion ratio"},
     {"kappaPyr", "Double", "~logNormal[log[2],0.25]","","C<->T Transition/transversion ratio"},
     {"A", "a", "LAMBDA"}},

    {{"GTR", "ExchangeModel[a]", "G"},
     {"SModel.gtr[S,A]"},
     {"S", "E", "exchange_prior", "A"},
     {"A", "a", "LAMBDA"}},

    {{"exchange_prior", "E"}, {"SModel.exchange_model"}},

    {{"E", "E", "P"}, {"SModel.constant_exchange_model"},{"*", "Double"}},

    {{"x3", "ExchangeModel[a]","G", "Triplets[a]"},
     {"SModel.singlet_to_triplet_exchange[A,submodel]"},
     {"submodel", "ExchangeModel[b]","","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"PAM", "ExchangeModel[AA]", "G"}, {"SModel.pam[A]"}, {"A", "a", "LAMBDA"}},

    {{"JTT", "ExchangeModel[AA]", "G"}, {"SModel.jtt[A]"}, {"A", "a", "LAMBDA"}},

    {{"WAG", "ExchangeModel[AA]", "G"}, {"SModel.wag[A]"}, {"A", "a", "LAMBDA"}},

    {{"LG", "ExchangeModel[AA]", "G"}, {"SModel.lg[A]"}, {"A", "a", "LAMBDA"}},

    {{"Empirical", "ExchangeModel[a]", "G"},
     {"SModel.empirical[A,filename]"},
     {"filename", "String"},
     {"A","a","LAMBDA"}},

    {{"M0", "ExchangeModel[Codon[a,b]]", "G", "Nucleotides[a],AminoAcids[b]","The Yang & Nielsen (2000) model of dN/dS"},
     {"SModel.m0[A,submodel,omega]"},
     {"omega", "Double", "~Uniform[0,1]","","Relative rate of non-synonymous changes relative to synonymous changes."},
     {"submodel", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
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

    // If we didn't have to perform things, I'd just make a mixture' that applies the alphabet to each sub-model in the list.
    // This is going to cause ordering trouble, since we will execute models after it is referenced.
    // If we had lazy evaluation, we wouldn't evaluate default args unless they were needed.

    {{"Mixture", "MixtureModel[a]","G"},
     {"SModel.scaled_mixture[Prelude.mapM[models,A],rates,frequencies]"},
     {"models", "List[MixtureModel[a]]",""},
     {"rates", "List[Double]", "~Dirichlet[Length[models],2]"},
     {"frequencies", "List[Double]", "~Dirichlet[Length[models],3]"},
     {"A","a","LAMBDA"}},

    // Let[var=E1,E2]::T
    // We need special handling of let, because it needs to take a variable name that is not fixed,
    // and because it needs to bind the variable name when parsing E2.
    // Also, if the let is of type T, then E2 needs to be of type T.

    {{"Length", "Int"},
     {"Prelude.length[x]"},
     {"x", "List[a]"}},

    // fraction ~ dirichlet' n (1 + n/2), rates ~ dirichlet' n 2
    {{"DP", "MixtureModel[a]","G"},
     {"SModel.dp[submodel,rates,frequencies]"},
     {"rates", "List[Double]", "~Dirichlet[n,2]"},
     {"frequencies", "List[Double]", "~Dirichlet[n,3]"},
     {"n","Int","4"},
     {"submodel", "MixtureModel[a]","","A"},
     {"A","a","LAMBDA"}},

    {{"MultiRate", "MixtureModel[a]", "G"},
     {"SModel.multi_rate_unif_bins[submodel,dist,n_bins]"},
     {"dist", "Distribution[Double]"},
     {"n_bins", "Int", "4"},
     {"submodel", "MixtureModel[a]","","A"},
     {"A","a","LAMBDA"}},

    {{"Rates.Gamma", "MixtureModel[a]", "G"},
     {"SModel.gamma_rates[submodel,alpha,n]"},
     {"n", "Int", "4", "", "The number of bins for discretizing the Gamma distribution."},
     {"alpha", "Double", "~logLaplace[-6,2]", "", "The shape parameter for the Gamma distribution."},
     {"submodel", "MixtureModel[a]", "", "A"},
     {"A", "a", "LAMBDA"}},

    {{"Rates.logNormal", "MixtureModel[a]", "G"},
     {"SModel.log_normal_rates[submodel,sigmaOverMu,n]"},
     {"n", "Int", "4", "", "The number of bins for discretizing the logNormal distribution"},
     {"sigmaOverMu", "Double", "~logLaplace[-3,1]"},
     {"submodel", "MixtureModel[a]", "", "A"},
     {"A", "a", "LAMBDA"}},
    
    {{"INV", "MixtureModel[a]", "G"},
     {"SModel.plus_inv[submodel,pInv]"},
     {"pInv","Double","~Uniform[0,1]", "", "The fraction of invariant sites."},
     {"submodel","MixtureModel[a]","","A"},
     {"A", "a", "LAMBDA"}},

    {{"M1a", "MixtureModel[Codon[a,b]]","G"},
     {"SModel.m1a[nuc_model,freq_model,omega1,p1,A]"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61","A"},
     {"omega1", "Double", "~Uniform[0,1]"},
     {"p1", "Double", "~Uniform[0,1]"},
     {"A","a","LAMBDA"}},
    
    {{"M2a", "MixtureModel[Codon[a,b]]","G"},
     {"SModel.m2a[nuc_model,freq_model,omega1,p1,posP,posW,A]"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]","The nucleotide exchange model."},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61","A","The frequency model."},
     {"omega1", "Double", "~Uniform[0,1]"},
     {"p1", "Double", "~Uniform[0,1]"},
     {"posP", "Double", "~Beta[1,10]" ,"", "The fraction of positively selected sites"},
     {"posW", "Double", "~logGamma[4,0.25]", "", "The dN/dS value for positively selected sites"},
     {"A","a","LAMBDA"}},

    {{"M2a_Test", "MixtureModel[Codon[a,b]]", "G"},
     {"SModel.m2a_test[nuc_model,freq_model,omega1,p1,posP,posW,posSelection,A]"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61","A"},
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

    {{"M3", "MixtureModel[Codon[a,b]]", "G"},
     {"SModel.m3[nuc_model,freq_model,ps,omegas,A]"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61", "A"},
     {"ps", "List[Double]", "~Dirichlet[n,2]"},
     {"omegas", "List[Double]", "~iid[n,Uniform[0,1]]"},
     {"n","Int","4"},
     {"A", "a", "LAMBDA"}},

    {{"M3_Test", "MixtureModel[Codon[a,b]]", "G"},
     {"SModel.m3_test[nuc_model,freq_model,ps,omegas,posP,posW,posSelection,A]"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61", "A"},
     {"ps", "List[Double]", "~Dirichlet[n,2]"},
     {"omegas", "List[Double]", "~iid[n,Uniform[0,1]]" },
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"posSelection", "Int", "~Bernoulli[0.5]"},
     {"n","Int","4"},
     {"A", "a", "LAMBDA"}},

    {{"M7", "MixtureModel[Codon[a,b]]", "G"},
     {"SModel.m7[nuc_model,freq_model,mu,gamma,n,A]"},
     {"n", "Int", "4"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"gamma", "Double", "~Beta[1,10]"},
     {"A", "a", "LAMBDA"}},
    
    {{"M8", "MixtureModel[Codon[a,b]]","G"},
     {"SModel.m8[nuc_model,freq_model,mu,gamma,n,posP,posW,A]"},
     {"n", "Int", "4"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"gamma", "Double", "~Beta[1,10]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"A","a","LAMBDA"}},

    {{"M8a", "MixtureModel[Codon[a,b]]","G"},
     {"SModel.m8a[nuc_model,freq_model,mu,gamma,n,posP,A]"},
     {"n", "Int", "4"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"gamma", "Double", "~Beta[1,10]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"A","a","LAMBDA"}},
    
    {{"M8a_Test", "MixtureModel[Codon[a,b]]","G"},
     {"SModel.m8a_test[nuc_model,freq_model,mu,gamma,n,posP,posW,posSelection,A]"},
     {"n", "Int", "4"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"gamma", "Double", "~Beta[1,10]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"posSelection", "Int", "~Bernoulli[0.5]"},
     {"A","a","LAMBDA"}},

    {{"branch-site", "MultiMixtureModel[Codon[a,b]]", "G"},
     {"SModel.branch_site_test[nuc_model,freq_model,fs,omegas,posP,posW,posSelection,A]"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61", "A"},
     {"fs", "List[Double]", "~Dirichlet[2,1]"},
     {"omegas", "List[Double]", "~iid[1,Uniform[0,1]]"},
     {"posP", "Double", "~Beta[1,10]"},
     {"posW", "Double", "~logGamma[4,0.25]"},
     {"posSelection", "Int", "~Bernoulli[0.5]"},
     {"A","a","LAMBDA"}},

    {{"dp_omega", "MixtureModel[Codon[a,b]]", "G"},
     {"SModel.dp_omega[nuc_model,freq_model,mu,omegas,A]"},
     {"nuc_model", "ExchangeModel[a]", "HKY", "Alphabet.getNucleotides[A]"},
     {"freq_model", "FrequencyModel[Codon[a,b]]", "F61", "A"},
     {"mu", "Double", "~Uniform[0,1]"},
     {"omegas", "List[Double]", "~Dirichlet[n,1]"},
     {"n","Int","4"},
     {"A","a","LAMBDA"}},
    
    {{"frequencies_prior", "F"}, {"SModel.frequencies_model"}},
    
    {{"Freq", "F", "P"}, {"SModel.constant_frequencies_model"},{"*", "Double"}},
    
    {{"Freq2", "F"}, {"SModel.constant_frequencies_model2[dict]"},{"dict", "List[Pair[String,Double]]"}},
    
    // We have A (the alphabet) available to reference in the default value for pi.  So we could do Dirichlet[n_letters[A],1.0]
    // How would we do a sampling rate just for the letter frequencies?
    {{"F", "FrequencyModel[a]", "G"},
     {"SModel.plus_f[A,pi]"},
     {"pi", "F", "frequencies_prior", "A"},
     {"A", "a", "LAMBDA"}},
    
    {{"F61", "FrequencyModel[Codon[a,b]]", "G"}, {"SModel.plus_f[A,pi]"},{"pi", "F", "frequencies_prior", "A"}, {"A", "a", "LAMBDA"}},

    {{"gwF", "FrequencyModel[a]", "G"}, {"SModel.plus_gwf[A,pi,f]"},{"pi", "F", "frequencies_prior", "A"},{"f", "Double", "~Uniform[0,1]"},{"A", "a", "LAMBDA"}},
    // How about a generic frequency model that is equivalent to fMutSel0?  Can we do that?
    // Or maybe we need to do fMutSel & fMutSel0 version of the site models.
    //These should really be Triplet models, not Codon models - so we need inheritance

    {{"F1x4", "FrequencyModel[Codon[a,b]]", "G"},
     {"SModel.f1x4[A,pi]"},
     {"pi", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"F3x4", "FrequencyModel[Codon[a,b]]", "G"},
     {"SModel.f3x4[A,pi1,pi2,pi3]"},
     {"pi1", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"pi2", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"pi3", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"MG94", "FrequencyModel[Codon[a,b]]", "G","The Muse & Gaut (1994) model of codon substitution."},
     {"SModel.mg94[pi,A]"},
     {"pi", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"MG94w9", "FrequencyModel[Codon[a,b]]", "G"},
     {"SModel.mg94w9[pi1,pi2,pi3,A]"},
     {"pi1", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"pi2", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"pi3", "F", "frequencies_prior","Alphabet.getNucleotides[A]"},
     {"A","a","LAMBDA"}},

    {{"DNA", "Alphabet", "N"}, {"dna"}},
    {{"RNA", "Alphabet"}, {"rna"}},
//    {{"C10", "MixtureModel[a]"}, {}},
//    {{"C20", "MixtureModel[a]"}, {}},
    {{"AA", "Alphabet"}, {"aa"}},
// We can't write Codons[a,b] yet because we don't any mechanism for dealing with inheritance
    {{"Codons", "Alphabet"}, {"codons[nuc,aa]"}, {"nuc", "Alphabet"}, {"aa", "Alphabet", "AA"}},
    {{"RCTMC", "RA[a]", "GN"},
     {"SModel.reversible_markov[Q,R]"},
     {"Q", "ExchangeModel[a]", "", "A"},
     {"R", "FrequencyModel[a]", "", "A"},
     {"A", "a", "LAMBDA"}},

    {{"UnitMixture", "MixtureModel[a]", "NG"},
     {"SModel.unit_mixture[submodel]"},
     {"submodel", "RA[a]", "", "A"},
     {"A", "a", "LAMBDA"},
    },

    {{"MultiMixtureModel", "MultiMixtureModel[a]", "NG"},
     {"SModel.mmm[submodel]"},
     {"submodel", "MixtureModel[a]", "", "A"},
     {"A", "a", "LAMBDA"}
    },

    {{"RS05", "IndelModel", "G"},
     {"IModel.rs05[logDelta,meanIndelLengthMinus1,tau,tree]"},
     {"logDelta", "Double", "~Laplace[-4,0.707]"},
     {"meanIndelLengthMinus1", "Double", "~Exponential[10]"},
     {"tau", "Double", "0.001"},
     {"tree", "tree", "LAMBDA"}
    },

    {{"RS07", "IndelModel", "G","Redelings & Suchard (2007) model of insertions and deletions."},
     {"IModel.rs07[logLambda,meanIndelLength,tree]"},
     {"logLambda", "Double", "~Laplace[-4,0.707]","","The log of the insertion-deletion rate, normalized to the substitution rate."},
     {"meanIndelLength", "Double", "Add[1.0,~Exponential[10]]"},
     {"tree", "tree", "LAMBDA"}
    },

    {{"RS07RelaxedRates", "IndelModel"}, {"IModel.rs07_relaxed_rates_model"}}
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

    // Add type constraints
    ptree constraints;
    if (s[0].size() >= 4)
	constraints = parse_constraints(s[0][3]);
    rule.push_back({"constraints", constraints});

    // Add function description
    if (s[0].size() >= 5 and not s[0][4].empty())
	rule.push_back({"description",ptree(s[0][4])});

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

	if (s[i].size() > 4 and not s[i][4].empty())
	    arg.push_back({"description",ptree(s[i][4])});

	args.push_back({"",arg});
    }
    rule.push_back({"args",args});
//    write_info(std::cout, rule);
    return rule;
}

vector<Rule> get_rules()
{
    vector<Rule> rules;
    for(auto& rule: all_default_arguments)
	rules.push_back(convert_rule(rule));
    return rules;
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
    // FIXME give info about function here?
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

