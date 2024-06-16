#include "util/file-paths.H"
#include "util/myexception.H"
#include "util/string/split.H"
#include "util/string/join.H"
#include <vector>

#include <filesystem>

using std::vector;
using std::string;
namespace fs = std::filesystem;

using std::optional;

optional<fs::path> check_file_in_path(const vector<fs::path>& paths, const fs::path& file_path)
{
    for(const auto& prefix: paths)
    {
	auto filename = prefix / file_path;
	if (not fs::exists(filename)) continue;
	return filename;
    }
    return {};
}

string show_path(const vector<fs::path>& path_list)
{
    // apparently using p.native() returns a string of wide characters on windows.
    vector<string> path_strings;
    for(auto& p: path_list)
        path_strings.push_back(p.string());
    return join(path_strings, ":");
}

fs::path find_file_in_path(const vector<fs::path>& path_list, const fs::path& file_path)
{
    auto path = check_file_in_path(path_list, file_path);
    if (not path)
	throw myexception()<<"Couldn't find file "<<file_path<<" in path '"<<show_path(path_list)<<"'";
    return *path;
}

#ifdef _WIN32
#include <windows.h>
#elif defined(__APPLE__)
#include <mach-o/dyld.h>
#include <climits>
#include <cstdlib>
#include <cstring>
#endif

fs::path find_exe_path(const fs::path& argv0)
{
    fs::path program_location;

#ifdef _WIN32
    constexpr int MAX_DIR_PATH = 2048;
    char buffer[MAX_DIR_PATH];

    GetModuleFileName( NULL, buffer, MAX_DIR_PATH );
    string path_string = buffer;
    program_location = path_string;
#elif defined(__APPLE__)
    constexpr int MAX_DIR_PATH = 2048;
    char buffer[MAX_DIR_PATH];

    uint32_t size = sizeof(path);
    if (_NSGetExecutablePath(buffer, &size) == 0)
    {
	path_string = buffer;
	program_location = path_string;
    }
    else
    {
	vector<char> buf(size);
	if (_NSGetExecutablePath(buf.data(), &size) == 0)
	{
	    path_string = heap_buf.data();
	    program_location = path_string;
	}
	else
	{
	    if (log_verbose) std::cerr<<"Failed to find exe path!\n";
	    return "";
	}
    }

#else
    /*
      Linux: readlink /proc/self/exe
      FreeBSD: sysctl CTL_KERN KERN_PROC KERN_PROC_PATHNAME -1
      Solaris: getexecname()
      BSD with procfs: readlink /proc/curproc/file
    */

    // This only works on Linux.
    if (fs::exists("/proc/self/exe"))
	program_location = "/proc/self/exe";
    // This only works on BSD with procfs.
    else if (fs::exists("/proc/curproc/file"))
	program_location = "/proc/curproc/file";
    // Try argv[0] - This *PROBABLY* works on windows.
    else if (fs::exists(argv0))
	program_location = argv0;
    // Search $PATH for argv[0]
    else if (not argv0.is_absolute() and getenv("PATH"))
    {
	string PATH = getenv("PATH");
	vector<fs::path> paths;
        for(auto& p: split(PATH,':'))
            paths.push_back(p);
	auto loc = check_file_in_path(paths, argv0);
	if (loc)
	    program_location = *loc;
    }
#endif

    program_location = canonical(program_location).parent_path();

    return program_location;
}

vector<fs::path> clean_paths(const vector<fs::path>& paths)
{
    vector<fs::path> paths2;
    for(int i=0;i<paths.size();i++)
	if (not paths[i].empty() and fs::exists(paths[i]))
	    paths2.push_back(paths[i]);
    return paths2;
}

std::optional<fs::path> get_home_dir()
{
    if (auto HOME = getenv("HOME"))
    {
        fs::path home_dir = HOME;
        return home_dir;
    }

    if (auto USERPROFILE = getenv("USERPROFILE"))
    {
        fs::path home_dir = USERPROFILE;
        return home_dir;
    }

    // We could also check HOMEDRIVE / HOMEPATH and HOMESHARE / HOMEPATH

    return {};
}

optional<fs::path> base_user_data_path()
{
// Possibly we should check if UNIX (including cygwin) and then use home.
    if (getenv("HOME"))
	return fs::path(getenv("HOME")) / ".local" / "share";
    else if (getenv("LOCALAPPDATA"))
	return fs::path(getenv("LOCALAPPDATA"));
    else
	return {};
}

