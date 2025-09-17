#include <filesystem>
#include <iostream>

#include "util/io.H"
#include "util/io/vector.H"
#include "util/string/convert.H"
#include "files.H"
#include "util/string/join.H"
#include "util/string/split.H"
#include "util/myexception.H"
#include "version.H"
#include "computation/loader.H"
#include "computation/haskell/ids.H"
#include <fmt/chrono.h>

using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::ofstream;
using std::ostream;
using std::shared_ptr;

namespace fs = std::filesystem;
namespace po = boost::program_options;
using po::variables_map;

fs::path create_unique_dir(const std::filesystem::path& dirbase)
{
    // FIXME. Maybe the ability to create arbitrary files should not be allowed?

    // 1. Create the parent directory.
    auto parent = dirbase.parent_path();
    if (not parent.empty())
	fs::create_directories(parent);

    // 2. Try to create the child directory
    for(int i=1;;i++)
    {
	auto dirname = dirbase;
	dirname += "-" + convertToString(i);

	// 3. Ensure a unique owner.
	if (fs::create_directory(dirname))
	    return dirname;
    }
}

string run_name(const variables_map& args)
{
    string name;
    if (args.count("name"))
	name = args["name"].as<string>();
    else if (args.count("align"))
    {
        std::set<fs::path> seen;
	vector<string> alignment_filenames;
	for(auto& filename_range: args["align"].as<vector<string> >())
	{
	    auto [name,range] = split_on_last(':',filename_range);
            auto p = fs::absolute( fs::path( name ) );
            auto filename = fs::path(name).filename().stem().string();
            if (not seen.count(p))
            {
                seen.insert(p);
                alignment_filenames.push_back(filename);
            }
	}
	name = join(alignment_filenames,'-');
    }
    else if (args.count("model"))
    {
        fs::path filepath = args["model"].as<vector<string>>()[0];
        name = filepath.filename().stem().string();
    }

    return name;
}

/// Create the directory for output files and return the name
fs::path init_dir(const variables_map& args)
{
    string name = run_name(args);
    
    fs::path dir = create_unique_dir(name);
    cerr<<"Created directory "<<dir<<" for output files."<<endl<<endl;
    return dir;
}

#if defined _MSC_VER || defined __MINGW32__
#include <windows.h>
#include <cerrno>
#include <process.h>

string hostname() 
{
    // We have to use MAX_COMPUTERNAME_LENGTH+1 so it doesn't fail in Win9x
    char temp[MAX_COMPUTERNAME_LENGTH + 1];
    DWORD size =  sizeof (temp);

    if (!GetComputerName (temp, &size))
	return "unknown";

    return string(temp);
}
#else
string hostname()
{
    string hostname="";
    char temp[256];
    if (not gethostname(temp,256))
	hostname = temp;
    return hostname;
}
#endif

string quote_string(const string& s,char q='"')
{
    if (s.empty() or s.find_first_of(" ;()") != string::npos)
	return string(1,q)+s+string(1,q);
    else
	return s;
}

inline void rtrim(std::string &s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

void run_info(json::object& info, int /*proc_id*/, int argc, char* argv[])
{
    json::array command;
    for(int i=0;i<argc;i++)
	command.push_back(argv[i]);

    auto now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    string start_time = fmt::format("{:%c}", *std::localtime(&now));
    rtrim(start_time);

    json::object env;
    for(auto& var: {"SLURM_JOBID", "JOB_ID", "LSB_JOBID"})
	if (auto evar = getenv(var))
	    env[var] = evar;

    info["command"] = command;
    info["directory"] = fs::current_path().string();
    info["start time"] = start_time;
    info["environment"] = env;
    info["pid"] = getpid();
    info["hostname"] = hostname();
    json::object program = version_info();
    program["name"] = "bali-phy";
    info["program"] = program;

#ifdef HAVE_MPI
    json mpi;
    mpi::communicator world;
    mpi["MPI_RANK"] = world.rank();
    mpi["MPI_SIZE"] = world.size();
    info["mpi"] = mpi;
#endif
}
