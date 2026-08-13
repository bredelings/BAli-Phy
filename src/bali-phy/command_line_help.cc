#include "command_line_help.H"

#include <algorithm>
#include <sstream>

#include "help.hh"
#include "paths.H"

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

/// Apply visibility filtering only to cumulative root help; direct command help remains complete.
string CommandLineHelpFormatter::make_help(const CLI::App* app, string name, CLI::AppFormatMode mode) const
{
    bool was_filtering = filtering;
    filtering = app == root;
    auto output = CLI::Formatter::make_help(app, std::move(name), mode);
    filtering = was_filtering;
    return output;
}

/// Remove options above the selected level before delegating their layout to CLI11.
string CommandLineHelpFormatter::make_group(string group, bool positional,
                                            vector<const CLI::Option*> options) const
{
    if (filtering)
        std::erase_if(options, [this](const auto* option) {return not visible(option);});
    if (options.empty()) return {};
    return CLI::Formatter::make_group(std::move(group), positional, std::move(options));
}

/// Expand the primary inference interface and list other commands visible at this level.
string CommandLineHelpFormatter::make_subcommands(const CLI::App* app, CLI::AppFormatMode mode) const
{
    if (not filtering or app != root)
        return CLI::Formatter::make_subcommands(app, mode);

    std::ostringstream output;
    if (inference_command and visible(inference_command))
    {
        output<<"\nInference:\n";
        output<<make_expanded(inference_command, CLI::AppFormatMode::Sub);
    }

    vector<const CLI::App*> commands;
    for(auto* command: app->get_subcommands({}))
        if (command != inference_command and not command->get_name().empty() and visible(command))
            commands.push_back(command);

    if (not commands.empty())
    {
        output<<"\nCommands:\n";
        for(auto* command: commands)
            output<<make_subcommand(command);
    }
    return output.str();
}

/// Explain how to move between cumulative levels without duplicating option declarations.
string CommandLineHelpFormatter::make_level_footer() const
{
    std::ostringstream output;
    output<<"\nShowing "<<to_string(level)<<" command-line options.";
    if (level != CommandHelpLevel::developer)
    {
        auto next = static_cast<CommandHelpLevel>(static_cast<int>(level) + 1);
        output<<" Not all options are shown!\n"
              <<"  Run `bali-phy help "<<to_string(next)<<"` to see more options.\n";
    }
    else
        output<<"\n";
    return output.str();
}

/// Reuse the existing package-aware topic listing in root command-line help.
string CommandLineHelpFormatter::make_topic_footer() const
{
    vector<string> path_arguments;
    if (package_paths and package_paths->count())
        path_arguments = package_paths->as<vector<string>>();

    std::ostringstream output;
    output<<"\nSee `bali-phy help TOPIC` for model-language and option help.\n\n";
    help_topics(output, get_package_paths(path_arguments));
    return output.str();
}

/// Add BAli-Phy's level-discovery and model-language topic guidance to root help only.
string CommandLineHelpFormatter::make_footer(const CLI::App* app) const
{
    auto footer = CLI::Formatter::make_footer(app);
    if (filtering and app == root)
        footer += make_level_footer()+make_topic_footer();
    return footer;
}
