#ifndef A_T_MODEL_H
#define A_T_MODEL_H

#include <vector>
#include <iostream>
#include <string>
#include <filesystem>

#include <boost/program_options.hpp>
#include "link-partitions.H"
#include "models/rules.H"                           // for Rules
#include "models/setup.H"
#include "util/owned-ptr.H"
#include "util/json.hh"
#include "tree/sequencetree.H"                      // for SequenceTree
class Parameters;
class Program;
class Model;
class alignment;
class module_loader;

void setup_heating(int proc_id, const boost::program_options::variables_map& args, Parameters& P);

void setup_partition_weights(const boost::program_options::variables_map& args, Parameters& P);

std::vector<model_t>
get_smodels(const boost::program_options::variables_map& args, const std::vector<alignment>& A,
	    shared_items<std::string>& smodel_names_mapping);

std::vector<model_t> 
get_imodels(const shared_items<std::string>& imodel_names_mapping, const SequenceTree& T);

void log_summary(std::ostream& out_cache, std::ostream& out_screen,std::ostream& out_both,
		 const shared_items<std::string>& imodels, const shared_items<std::string>& smodels,
		 const Parameters& P,const boost::program_options::variables_map& args);

void set_foreground_branches(Parameters& P, const SequenceTree& T);

std::tuple<Program, json> create_A_and_T_model(const Rules& R, boost::program_options::variables_map& args, const std::shared_ptr<module_loader>& L,
                                               std::ostream& out_cache, std::ostream& out_screen, std::ostream& out_both,
                                               int proc_id, const std::filesystem::path& dir);

void write_initial_alignments(boost::program_options::variables_map& args, int proc_id, const std::filesystem::path& dir_name);

SequenceTree load_constraint_tree(const std::string& filename,const std::vector<std::string>&);

#endif
