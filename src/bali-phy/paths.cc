#include "paths.H"
#include "util/file-paths.H"
#include "util/myexception.H"
#include "util/string/split.H"
#include "util/string/join.H"
#include <vector>

using std::vector;
using std::string;
namespace fs = std::filesystem;

using boost::program_options::variables_map;
using std::optional;

optional<fs::path> get_system_lib_path(const string& exe_name)
{
    fs::path system_lib_path = find_exe_path(exe_name);
    if (not system_lib_path.empty())
    {
	system_lib_path = system_lib_path.parent_path() / "lib" / "bali-phy";

	if (not fs::exists(system_lib_path)) return {};
    }
    return system_lib_path;
}

optional<fs::path> get_user_lib_path()
{
    fs::path user_lib_path;
    if (getenv("HOME"))
    {
	user_lib_path = getenv("HOME");
	user_lib_path = user_lib_path / ".local" / "share" / "bali-phy" / "packages";

	if (not fs::exists(user_lib_path)) return {};
    }
    return user_lib_path;
}

vector<fs::path> get_package_paths(const string& argv0, variables_map& args)
{
    vector<fs::path> paths;

    // 1. First add the user-specified package paths
    if (args.count("package-path"))
	for(const string& path: split(args["package-path"].as<string>(),':'))
	    paths.push_back(path);

    // 2. Then add the user package directories
    if (auto p = get_user_lib_path())
        paths.push_back(*p);

    // 3. Finally add the default system paths
    if (auto p = get_system_lib_path(argv0))
        paths.push_back(*p);

    return clean_paths(paths);
}
