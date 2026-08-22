#ifndef A_T_PROG_H
#define A_T_PROG_H

#include <memory>                                   // for shared_ptr, alloc...
#include <filesystem>                               // for std::filesystem::path
#include <optional>                                 // for optional
#include <string>                                   // for string
#include <tuple>                                    // for tuple
#include <vector>                                   // for vector
#include "bali-phy/cmd_line.hh"                     // for InferOptions
#include "computation/program.hh"                    // for Program

#include "computation/haskell/haskell.hh"            // for Hs::Exp

class model_t;
class module_loader;

std::unique_ptr<Program>
gen_atmodel_program(const InferOptions& options,
		    const std::shared_ptr<module_loader>& L,
		    const std::filesystem::path& output_directory,
		    const std::filesystem::path& program_filename,
		    const std::vector<Hs::Exp>& alphabet_exps,
		    const std::vector<std::pair<std::filesystem::path,std::string>>& filename_ranges,
		    int n_leaves,
		    const model_t& decls,
		    const std::vector<model_t>& SMs,
		    const std::vector<std::optional<int>>& s_mapping,
                    const std::vector<std::string>& s_conditions,
		    const std::vector<model_t>& IMs,
		    const std::vector<std::optional<int>>& i_mapping,
		    const std::vector<model_t>& scaleMs,
		    const std::vector<std::optional<int>>& scale_mapping,
		    const model_t& tree_model,
		    const model_t& subst_rates_model,
		    const model_t& indel_rates_model);

#include "sequence/alphabet.hh"

Hs::Exp get_alphabet_expression(const alphabet& a);

std::map<std::string, std::string> get_fixed(const InferOptions& options);

#endif
