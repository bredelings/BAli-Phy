#ifndef FILES_H
#define FILES_H

#include <vector>
#include <string>
#include <fstream>

#include <filesystem>
#include "cmd_line.H"
#include "util/json.hh"

std::filesystem::path create_unique_dir(const std::filesystem::path& dirbase);

std::filesystem::path init_dir(const InferOptions& options);

void run_info(json::object& j, int proc_id, int argc,char* argv[]);

std::string run_name(const InferOptions& options);
#endif
