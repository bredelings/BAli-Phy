#include "command_line_help.hh"

#include <algorithm>
#include <sstream>

#include "help.hh"
#include "paths.hh"
#include "util/text.hh"

using std::string;
using std::vector;

/// Return the command word accepted by `bali-phy help LEVEL`.
string to_string(CommandHelpLevel level)
{
    switch(level)
    {
        case CommandHelpLevel::basic: return "basic";
        case CommandHelpLevel::advanced: return "advanced";
        case CommandHelpLevel::expert: return "expert";
        case CommandHelpLevel::developer: return "developer";
    }
    std::terminate();
}

/// An unclassified option remains available to parsing but is shown only at developer level.
bool CommandLineHelpFormatter::visible(const CLI::Option* option) const
{
    auto found = option_levels.find(option);
    auto minimum_level = found == option_levels.end() ? CommandHelpLevel::developer : found->second;
    return minimum_level <= level;
}

/// An unclassified command remains available directly but is shown only at developer level.
bool CommandLineHelpFormatter::visible(const CLI::App* command) const
{
    auto found = command_levels.find(command);
    auto minimum_level = found == command_levels.end() ? CommandHelpLevel::developer : found->second;
    return minimum_level <= level;
}

void CommandLineHelpFormatter::show_at(const CLI::Option* option, CommandHelpLevel minimum_level)
{
    option_levels[option] = minimum_level;
}

void CommandLineHelpFormatter::show_at(const CLI::App* command, CommandHelpLevel minimum_level)
{
    command_levels[command] = minimum_level;
}

/// Register the unnamed CLI11 option group that represents BAli-Phy's default operation.
void CommandLineHelpFormatter::set_default_command(const CLI::App* command, string synopsis)
{
    default_command = command;
    default_synopsis = std::move(synopsis);
}

void CommandLineHelpFormatter::set_synopsis(const CLI::App* command, string synopsis)
{
    command_synopses[command] = std::move(synopsis);
}

/// Present an internally root-owned option as part of one command's interface.
void CommandLineHelpFormatter::show_with(const CLI::App* command, const CLI::Option* option,
                                         CommandHelpLevel minimum_level)
{
    show_at(option, minimum_level);
    option_commands[option] = command;
    extra_command_options[command].push_back(option);
}

/// Return one command's positional and named options in a single display order.
vector<const CLI::Option*>
CommandLineHelpFormatter::command_options(const CLI::App* command, bool include_help,
                                          bool include_positionals) const
{
    auto local_options = command->get_options();
    if (auto* parent = command->get_parent())
    {
        // CLI11 2.7 includes parent options here for fallthrough commands.  They remain accepted by the
        // parser, but the formatter presents them once in the separate global-options section.
        auto parent_options = parent->get_options();
        std::erase_if(local_options, [&parent_options](const auto* option) {
            return std::find(parent_options.begin(), parent_options.end(), option) != parent_options.end();
        });
    }

    vector<const CLI::Option*> options;
    const auto add = [this, &options](const CLI::Option* option)
    {
        if (not filtering or visible(option)) options.push_back(option);
    };

    if (include_positionals)
        for(auto* option: local_options)
            if (option->get_positional()) add(option);

    if (auto extras = extra_command_options.find(command); extras != extra_command_options.end())
        for(auto* option: extras->second) add(option);

    for(auto* option: local_options)
        if (not option->get_positional() and
            (include_help or option != command->get_help_ptr()) and
            not option_commands.contains(option))
            add(option);

    return options;
}

/// Render a mixed list while retaining CLI11's established option-line layout.
string CommandLineHelpFormatter::make_option_section(const string& title,
                                                     const vector<const CLI::Option*>& options) const
{
    if (options.empty()) return {};

    std::ostringstream output;
    output<<"\n"<<title<<":\n";
    for(auto* option: options)
        output<<make_option(option, option->get_positional());
    return output.str();
}

string CommandLineHelpFormatter::make_command_options(const CLI::App* command, bool include_help,
                                                      bool include_positionals) const
{
    string title = command->get_name();
    if (not title.empty()) title[0] = std::toupper(static_cast<unsigned char>(title[0]));
    return make_option_section(title+" options", command_options(command, include_help, include_positionals));
}

/// Apply cumulative filtering to root help; direct command help remains complete.
string CommandLineHelpFormatter::make_help(const CLI::App* app, string name, CLI::AppFormatMode mode) const
{
    bool was_filtering = filtering;
    auto* previous_app = current_app;
    filtering = app == root;
    current_app = app;
    auto output = CLI::Formatter::make_help(app, std::move(name), mode);
    if (filtering)
        output += make_post_options_help();
    current_app = previous_app;
    filtering = was_filtering;
    return output;
}

/// List concrete command forms instead of CLI11's generic SUBCOMMAND placeholder.
string CommandLineHelpFormatter::make_usage(const CLI::App* app, string name) const
{
    if (app != root)
    {
        if (auto synopsis = command_synopses.find(app); synopsis != command_synopses.end())
            return "Usage:\n  "+root->get_name()+" [OPTIONS] "+synopsis->second+"\n"+
                   make_option_section("Global options", command_options(root, false, true));
        return CLI::Formatter::make_usage(app, std::move(name));
    }

    std::ostringstream output;
    output<<"Usage:\n";
    if (default_command)
        output<<"  "<<name<<" [OPTIONS] "<<default_synopsis<<"\n";
    for(auto* command: app->get_subcommands({}))
        if (not command->get_name().empty() and visible(command))
            output<<"  "<<name<<" [OPTIONS] "<<command_synopses.at(command)<<"\n";
    return output.str();
}

/// Positional arguments are rendered with their command's named options.
string CommandLineHelpFormatter::make_positionals(const CLI::App* app) const
{
    if (command_synopses.contains(app)) return {};
    return CLI::Formatter::make_positionals(app);
}

/// Positionals already have semantic names, so show only their repetition rather than type or REQUIRED labels.
string CommandLineHelpFormatter::make_option_opts(const CLI::Option* option) const
{
    if (option->get_positional())
        return option->get_expected_max() == CLI::detail::expected_max_vector_size ? " ..." : "";
    return CLI::Formatter::make_option_opts(option);
}

/// Remove options above the selected level before delegating their layout to CLI11.
string CommandLineHelpFormatter::make_group(string group, bool positional,
                                            vector<const CLI::Option*> options) const
{
    if (current_app != root and command_synopses.contains(current_app) and group == "OPTIONS")
        return make_command_options(current_app, true, true);

    if (filtering)
        std::erase_if(options, [this](const auto* option) {
            return not visible(option) or option_commands.contains(option);
        });
    if (options.empty()) return {};
    if (current_app == root and group == "OPTIONS") group = "Global options";
    return CLI::Formatter::make_group(std::move(group), positional, std::move(options));
}

/// Follow the usage synopses with the options belonging to each visible command.
string CommandLineHelpFormatter::make_subcommands(const CLI::App* app, CLI::AppFormatMode mode) const
{
    if (not filtering or app != root)
        return CLI::Formatter::make_subcommands(app, mode);

    std::ostringstream output;
    if (default_command)
        output<<make_option_section("Infer options", command_options(default_command, false, false));
    for(auto* command: app->get_subcommands({}))
        if (not command->get_name().empty() and visible(command))
            output<<make_command_options(command, false, false);
    return output.str();
}

/// Explain how to move between cumulative levels without duplicating option declarations.
string CommandLineHelpFormatter::make_level_guidance() const
{
    std::ostringstream output;
    output<<"\nShowing "<<bold(to_string(level))<<" command line options.";
    if (level != CommandHelpLevel::developer)
    {
        auto next = static_cast<CommandHelpLevel>(static_cast<int>(level) + 1);
        output<<"  Not all options are shown!\n"
              <<"  * See `bali-phy help "<<bold(to_string(next))<<"` to see more options.\n";
    }
    else
        output<<"\n";

    if (level != CommandHelpLevel::basic)
    {
        auto previous = static_cast<CommandHelpLevel>(static_cast<int>(level) - 1);
        if (previous == CommandHelpLevel::basic)
            output<<"  * See `bali-phy help` to see fewer options.\n";
        else
            output<<"  * See `bali-phy help "<<bold(to_string(previous))<<"` to see fewer options.\n";
    }
    return output.str();
}

/// Reuse the existing package-aware topic listing in root command-line help.
string CommandLineHelpFormatter::make_topic_guidance() const
{
    vector<string> path_arguments;
    if (package_paths and package_paths->count())
        path_arguments = package_paths->as<vector<string>>();

    std::ostringstream output;
    output<<"\nSee `bali-phy help "<<underline("option")<<"` for help on "<<underline("option")<<".  For example,\n"
          <<"  * `bali-phy help "<<bold("alphabet")<<"` shows help on the "<<bold("--alphabet")<<" command.\n"
          <<"  * `bali-phy help "<<bold("Normal")<<"` shows help on the normal distribution.\n"
          <<"  * `bali-phy help "<<bold("TN93")<<"` shows help on the TN93 model.\n"
          <<"  * `bali-phy help "<<bold("log")<<"` shows help on the log function.\n\n";
    help_topics(output, get_package_paths(path_arguments));
    return output.str();
}

/// Add BAli-Phy's preformatted guidance after CLI11 has rendered its own help.
string CommandLineHelpFormatter::make_post_options_help() const
{
    return make_level_guidance()+make_topic_guidance();
}
