#ifndef FILES_H
#define FILES_H

#include <vector>
#include <string>
#include <fstream>
#include <iostream>

#include <boost/filesystem/operations.hpp>
#include <boost/program_options.hpp>

void close_files(std::vector<boost::shared_ptr<std::ofstream>>& files);

void delete_files(std::vector<std::string>& filenames);

std::vector<boost::shared_ptr<std::ofstream>> open_files(int proc_id, const std::string& name, std::vector<std::string>& names);

std::string open_dir(const std::string& dirbase);

std::string init_dir(const boost::program_options::variables_map& args);

std::vector<boost::shared_ptr<std::ostream>> 
init_files(int proc_id, const std::string& dirname,int argc,char* argv[]);

#endif
