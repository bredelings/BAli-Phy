#ifndef LOGGERS_H
#define LOGGERS_H

#include <vector>
#include <set>
#include <string>
#include <filesystem>
#include "mcmc/mcmc.H"                              // for MCMC::Logger
#include "util/json.hh"                             // for json
#include "util/owned-ptr.H"                         // for owned_ptr

#include <boost/program_options.hpp>

// used to (i) determine what to print for --test in bali-phy.cc
//        (ii) describe log file name in bali-phy.cc
//       (iii) determine what log files to create in A-T-model.cc
std::set<std::string> get_log_formats(const boost::program_options::variables_map& args, bool is_A_T_model);

#endif
