#ifndef BALI_PHY_COMMAND_LINE_HELP_H
#define BALI_PHY_COMMAND_LINE_HELP_H

#include <map>
#include <string>
#include <vector>

#include <CLI/CLI.hpp>

enum class CommandHelpLevel
{
    basic,
    advanced,
    expert,
    developer
};

/// Filter one CLI11 command tree by BAli-Phy's cumulative help levels.
class CommandLineHelpFormatter : public CLI::Formatter
{
    const CLI::App* root;
    const CLI::Option* package_paths = nullptr;
    std::map<const CLI::Option*, CommandHelpLevel> option_levels;
    std::map<const CLI::App*, CommandHelpLevel> command_levels;
    std::map<const CLI::App*, std::string> command_synopses;
    std::map<const CLI::Option*, const CLI::App*> option_commands;
    std::map<const CLI::App*, std::vector<const CLI::Option*>> extra_command_options;
    mutable CommandHelpLevel level = CommandHelpLevel::basic;
    mutable bool filtering = false;
    mutable const CLI::App* current_app = nullptr;

    bool visible(const CLI::Option* option) const;
    bool visible(const CLI::App* command) const;
    std::vector<const CLI::Option*> command_options(const CLI::App* command, bool include_help) const;
    std::string make_option_section(const std::string& title,
                                    const std::vector<const CLI::Option*>& options) const;
    std::string make_command_options(const CLI::App* command, bool include_help) const;
    std::string make_level_guidance() const;
    std::string make_topic_guidance() const;
    std::string make_post_options_help() const;

public:
    explicit CommandLineHelpFormatter(const CLI::App* root_app):root(root_app) {}

    void set_package_paths(const CLI::Option* option) {package_paths = option;}
    void set_level(CommandHelpLevel new_level) {level = new_level;}
    void show_at(const CLI::Option* option, CommandHelpLevel minimum_level);
    void show_at(const CLI::App* command, CommandHelpLevel minimum_level);
    void set_synopsis(const CLI::App* command, std::string synopsis);
    void show_with(const CLI::App* command, const CLI::Option* option,
                   CommandHelpLevel minimum_level);

    std::string make_help(const CLI::App* app, std::string name, CLI::AppFormatMode mode) const override;
    std::string make_usage(const CLI::App* app, std::string name) const override;
    std::string make_positionals(const CLI::App* app) const override;
    std::string make_option_opts(const CLI::Option* option) const override;
    std::string make_group(std::string group, bool positional,
                           std::vector<const CLI::Option*> options) const override;
    std::string make_subcommands(const CLI::App* app, CLI::AppFormatMode mode) const override;
};

std::string to_string(CommandHelpLevel level);

#endif
