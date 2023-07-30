#ifndef FILES_H
#define FILES_H

#include <vector>
#include <string>
#include <fstream>

#include <boost/program_options.hpp>
#include <filesystem>
#include "util/json.hh"

std::filesystem::path init_dir(const boost::program_options::variables_map& args);

void run_info(json& j, int proc_id, int argc,char* argv[]);

#endif
