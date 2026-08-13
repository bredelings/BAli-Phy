#ifndef A_T_MODEL_H
#define A_T_MODEL_H

#include <vector>
#include <iostream>
#include <string>
#include <filesystem>

#include "cmd_line.H"
#include "link-partitions.H"
#include "models/rules.H"                           // for Rules
#include "models/compile.H"
#include "util/owned-ptr.H"
#include "util/json.hh"
#include "tree/sequencetree.H"                      // for SequenceTree
class Program;
class module_loader;

std::tuple<std::unique_ptr<Program>, json::object>
create_A_and_T_model(const Rules& R,
                     const InferOptions& options,
                     bool test,
                     int verbosity,
                     const std::shared_ptr<module_loader>& L,
		     int proc_id,
                     const std::filesystem::path& dir);

void write_initial_alignments(const InferOptions& options,
                              int proc_id,
                              const std::filesystem::path& dir_name);

SequenceTree load_constraint_tree(const std::string& filename,const std::vector<std::string>&);

#endif
