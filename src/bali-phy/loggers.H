#ifndef LOGGERS_H
#define LOGGERS_H

#include <vector>
#include <set>
#include <string>
#include <filesystem>
#include "mcmc/mcmc.H"                              // for MCMC::Logger
#include "util/json.hh"                             // for json
#include "util/owned-ptr.H"                         // for owned_ptr
#include "mcmc/logger.H"
class Model;
namespace MCMC { template <typename T> class TableFunction; }

#include <boost/program_options.hpp>

// only used to print the TSV output for --test in bali-phy.cc
std::string table_logger_line(const Model& M);

// only used to print JSON output for --test in bali-phy.cc
json logged_params_and_some_computed_stuff(const Model& M, const json& jlog, long t);

// used to (i) determine what to print for --test in bali-phy.cc
//        (ii) describe log file name in bali-phy.cc
//       (iii) determine what log files to create in A-T-model.cc
std::set<std::string> get_log_formats(const boost::program_options::variables_map& args, bool is_A_T_model);

#endif
