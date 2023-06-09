#ifndef A_T_PROG_H
#define A_T_PROG_H

#include <memory>                                   // for shared_ptr, alloc...
#include <filesystem>                               // for std::filesystem::path
#include <optional>                                 // for optional
#include <string>                                   // for string
#include <tuple>                                    // for tuple
#include <vector>                                   // for vector
#include "computation/program.H"                    // for Program

#include "computation/expression/expression_ref.H"  // for expression_ref
#include "models/model.H"

class model_t;
class module_loader;

Program gen_atmodel_program(const std::shared_ptr<module_loader>& L,
                            const Model::key_map_t& k,
                            const std::filesystem::path& program_filename,

                            const std::vector<expression_ref>& alphabet_exps,
                            const std::vector<std::pair<std::string,std::string>>& filename_ranges,
                            int n_leaves,
                            const std::vector<model_t>& SMs,
                            const std::vector<std::optional<int>>& s_mapping,
                            const std::vector<model_t>& IMs,
                            const std::vector<std::optional<int>>& i_mapping,
                            const std::vector<model_t>& scaleMs,
                            const std::vector<std::optional<int>>& scale_mapping,
                            const model_t& branch_length_model,
                            const std::vector<int>& like_calcs);

#include "sequence/alphabet.H"

expression_ref get_alphabet_expression(const alphabet& a);

#endif
