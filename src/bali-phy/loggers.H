#ifndef LOGGERS_H
#define LOGGERS_H

#include <vector>
#include <set>
#include <string>
#include <memory>
#include "models/model.H"
#include "util/owned-ptr.H"
#include "mcmc/logger.H"
#include "mcmc/mcmc.H" // for MCMC::Logger

#include <boost/program_options.hpp>

std::string table_logger_line(MCMC::TableFunction<std::string>& TF, const Model& M, long t);

std::set<std::string> get_log_formats(const boost::program_options::variables_map& args, bool is_A_T_model);

std::vector<MCMC::Logger> construct_loggers(const boost::program_options::variables_map& args, owned_ptr<Model>& M, int subsample, const std::vector<std::string>& Rao_Blackwellize, int proc_id, const std::string& dir_name);

owned_ptr<MCMC::TableFunction<std::string>> construct_table_function(owned_ptr<Model>& M, const std::vector<std::string>& Rao_Blackwellize);
#endif
