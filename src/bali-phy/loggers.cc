#include <set>

#include "loggers.H"
#include "tools/parsimony.H"
#include "tools/stats-table.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"
#include "computation/expression/var.H"
#include "mcmc/mcmc.H"
#include "mcmc/logger.H"
#include "substitution/parsimony.H"
#include "models/parse.H"
#include "util/mapping.H"
#include "util/string/join.H"
#include "util/string/split.H"
#include "util/json.hh"

namespace fs = std::filesystem;

extern int log_verbose;

using std::vector;
using std::map;
using std::string;
using std::set;
using std::shared_ptr;
using std::make_shared;

using std::to_string;

using std::optional;

set<string> get_log_formats(const boost::program_options::variables_map& args, bool is_A_T_model)
{
    string log_format = is_A_T_model ? "tsv" : "json";
    if (args.count("log-format"))
        log_format = args["log-format"].as<string>();
    auto log_formats_vec = split(log_format,',');
    set<string> log_formats;
    for(auto& format: log_formats_vec)
        log_formats.insert(format);
    return log_formats;
}
