#ifndef STARTUP_CMD_LINE_H
#define STARTUP_CMD_LINE_H

#include <filesystem>
#include <optional>
#include <set>
#include <string>
#include <variant>
#include <vector>

struct CompilerOptions
{
    bool dump_parsed = false;
    bool dump_renamed = false;
    bool dump_typechecked = false;
    bool dump_desugared = false;
    bool dump_optimized = false;
    std::optional<std::string> recompile;
    bool optimize = true;
    bool fully_lazy = true;
    bool pre_inline = true;
    bool post_inline = true;
    bool let_float_from_case = true;
    bool let_float_from_apply = true;
    bool let_float_from_let = true;
    bool case_of_constant = true;
    bool case_of_variable = true;
    bool case_of_case = true;
    int inline_threshold = 8;
    bool beta_reduction = true;
    int simplifier_max_iterations = 4;
    bool dump_ffi = false;
    bool force_cpp = false;
    std::vector<std::string> cpp_definitions;
    std::vector<std::string> cpp_undefinitions;
    bool dump_cpp = false;
};

struct GlobalOptions
{
    int verbosity = 0;
    // Test mode changes process-wide output behavior even though --test belongs to infer.
    bool test = false;
    std::vector<std::string> package_paths;
    std::vector<std::string> settings;
    std::optional<unsigned long> seed;
    CompilerOptions compiler;
};

struct InferOptions
{
    std::optional<std::filesystem::path> config_file;
    std::vector<std::string> alignments;
    std::optional<long int> iterations;
    std::optional<std::string> name;
    int subsample = 1;
    std::optional<std::string> log_format;
    int pre_burnin = 3;
    std::optional<std::string> enable;
    std::optional<std::string> disable;
    std::optional<std::string> beta;
    std::optional<std::string> dbeta;
    std::optional<std::string> tree;
    bool unalign = false;
    std::vector<std::string> alphabets;
    std::vector<std::string> smodels;
    std::vector<std::string> imodels;
    std::vector<std::string> scales;
    std::vector<std::string> fixed;
    std::vector<std::string> variables;
    std::vector<std::string> links;
    std::string subst_rates = "constant";
    std::string indel_rates = "relaxed";
    std::optional<std::string> partition_weights;
    std::optional<std::string> topology_constraint;
    std::optional<std::string> alignment_constraint;
    std::optional<std::string> align_constraint;
    std::optional<std::string> likelihood_calculators;
};

struct RunCommand
{
    std::filesystem::path program;
    std::vector<std::string> arguments;
};

struct PrintCommand
{
    std::string expression;
    std::vector<std::string> alphabets;
};

struct TypeCommand
{
    std::string name;
};

struct TestModuleCommand
{
    std::string module;
};

struct HelpCommand
{
    std::optional<std::string> topic;
};

using Command = std::variant<InferOptions, RunCommand, PrintCommand, TypeCommand, TestModuleCommand, HelpCommand>;

struct CommandLine
{
    GlobalOptions global;
    Command command;
};

CommandLine parse_cli11_command_line(int argc, char* argv[]);
std::string get_command_line(int argc, char* argv[]);

std::set<std::string> get_log_formats(const InferOptions& options, bool is_A_T_model);
#endif
