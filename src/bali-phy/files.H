#ifndef FILES_H
#define FILES_H

#include <vector>
#include <string>
#include <fstream>

#include <boost/program_options.hpp>
#include <filesystem>
#include "util/json.hh"

void close_files(std::vector<std::shared_ptr<std::ofstream>>& files);

std::filesystem::path init_dir(const boost::program_options::variables_map& args);

void run_info(json& j, int proc_id, int argc,char* argv[]);

std::vector<std::shared_ptr<std::ostream>> 
init_files(int proc_id, const std::filesystem::path& dirname,int argc,char* argv[]);

#endif
