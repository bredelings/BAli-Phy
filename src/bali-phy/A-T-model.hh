#ifndef A_T_MODEL_H
#define A_T_MODEL_H

#include <vector>
#include <iostream>
#include <string>
#include <filesystem>

#include "cmd_line.hh"
#include "link-partitions.hh"
#include "models/rules.hh"                           // for Rules
#include "models/compile.hh"
#include "util/owned-ptr.hh"
#include "util/json.hh"
class Program;
class module_loader;

std::tuple<std::unique_ptr<Program>, json::object>
create_A_and_T_model(const Rules& R,
                     const InferOptions& options,
                     const std::shared_ptr<module_loader>& L,
		     int proc_id,
                     const std::filesystem::path& dir);

void write_initial_alignments(const InferOptions& options,
                              int proc_id,
                              const std::filesystem::path& dir_name);

#endif
