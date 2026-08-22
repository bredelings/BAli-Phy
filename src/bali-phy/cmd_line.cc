#include "cmd_line.hh"
#include "util/string/join.hh"
#include "util/string/split.hh"

using std::string;
using std::vector;
using std::set;

string get_command_line(int argc, char* argv[])
{
    vector<string> args;
    for(int i=0;i<argc;i++)
	args.push_back(argv[i]);

    return join(args," ");
}

set<string> get_log_formats(const InferOptions& options, bool is_A_T_model)
{
    string log_format = is_A_T_model ? "tsv" : "json";
    if (options.log_format)
        log_format = *options.log_format;
    auto log_formats_vec = split(log_format,',');
    set<string> log_formats;
    for(auto& format: log_formats_vec)
        log_formats.insert(format);
    return log_formats;
}
