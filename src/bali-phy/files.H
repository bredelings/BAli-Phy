#ifndef FILES_H
#define FILES_H

#include <vector>
#include <string>
#include <fstream>

#include <boost/program_options.hpp>
#include <filesystem>
#include "util/json.hh"

std::filesystem::path create_unique_dir(const std::filesystem::path& dirbase);

std::filesystem::path init_dir(const boost::program_options::variables_map& args);

void run_info(json::object& j, int proc_id, int argc,char* argv[]);

std::string run_name(const boost::program_options::variables_map& args);
#endif
