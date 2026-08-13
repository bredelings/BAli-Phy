#include "command_config.H"

#include <regex>
#include <sstream>

#include "util/io.H"
#include "util/myexception.H"

using std::string;

namespace
{

const std::regex simple_option(R"config(:([^ \t]+)[ \t]+([^ \t"].*)[ \t]*)config");
const std::regex quoted_option(R"config(:([^ \t]+)[ \t]+"(([^\\"]|\\.)*)"[ \t]*)config");
const std::regex option_without_argument(R"config(:([^ \t]+)[ \t]*)config");
const std::regex comment(R"config([ \t]*#.*)config");

/// Interpret the escape sequences accepted in quoted command-file option values.
string unescape_option_value(const string& value, const string& filename, std::size_t line_number)
{
    std::ostringstream output;
    for(std::size_t i = 0; i < value.size(); i++)
    {
        char c = value[i];
        if (c == '\\')
        {
            c = value[++i];
            if (c == 'n')
                c = '\n';
            else if (c == 't')
                c = '\t';
            else if (c != '\\' and c != '"')
                throw myexception()<<filename<<":"<<line_number<<": invalid escape sequence '\\"<<c
                                   <<"' in option value \""<<value<<"\"";
        }
        output<<c;
    }
    return output.str();
}

}

/// Read the established command-file syntax without assigning values to a particular parser.
CommandConfig read_command_config(std::istream& file, const string& filename)
{
    CommandConfig config;
    std::ostringstream model_source;

    string line;
    std::size_t line_number = 0;
    while(portable_getline(file, line))
    {
        line_number++;
        std::smatch match;

        if (std::regex_match(line, match, comment))
            continue;
        if (not line.starts_with(':'))
        {
            model_source<<line<<"\n";
            continue;
        }

        string name;
        if (std::regex_match(line, match, option_without_argument))
            name = match[1];
        else if (std::regex_match(line, match, quoted_option))
        {
            name = match[1];
            config.options[name].push_back(unescape_option_value(match[2], filename, line_number));
        }
        else if (std::regex_match(line, match, simple_option))
        {
            name = match[1];
            config.options[name].push_back(match[2]);
        }
        else
            throw myexception()<<filename<<":"<<line_number<<": malformed line '"<<line
                               <<"'; expected ':option value' or ':option \"value\"'";

        config.options.try_emplace(name);
        config.option_lines.try_emplace(name, line_number);
    }

    config.model_source = model_source.str();
    return config;
}
