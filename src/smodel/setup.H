#ifndef SMODEL_SETUP_H
#define SMODEL_SETUP_H
#include <vector>
#include <valarray>
#include <string>
#include <boost/program_options.hpp>
#include "object.H"
#include "computation/formula_expression.H"
#include "computation/loader.H"
#include "alignment/alignment.H"
#include "sequence/alphabet.H"

/// Estimate the empirical frequencies of different letters from the alignment, with pseudocounts
std::valarray<double> empirical_frequencies(const boost::program_options::variables_map& args,
					    const alignment& A);

/// Estimate the empirical frequencies of different letters from the alignment, with pseudocounts
std::valarray<double> empirical_frequencies(const boost::program_options::variables_map& args,
					    const std::vector<alignment>& A);

/// Return a substitution::Model specified by args
formula_expression_ref get_smodel(const module_loader& L,
				  const boost::program_options::variables_map& args,
				  const object_ptr<const alphabet>& a,
				  const boost::shared_ptr<const std::valarray<double> >& default_frequencies);

/// Return a substitution::Model specified by name
formula_expression_ref get_smodel(const module_loader& L,
				  const boost::program_options::variables_map& args,
				  const std::string&,
				  const alignment& A);

/// Return a substitution::Model specified by args
formula_expression_ref get_smodel(const module_loader& L,
				  const boost::program_options::variables_map& args,
				  const alignment& A);

/// Return a substitution::Model specified by name
formula_expression_ref get_smodel(const module_loader& L,
				  const boost::program_options::variables_map& args,
				  const std::string&,
				  const std::vector<alignment>& A);

std::string default_markov_model(const alphabet& a);

#endif
