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
    vector<string> path_strings;
    for(auto& p: path_list)
        path_strings.push_back(p.native());
    return join(path_strings, ":");
}

fs::path find_file_in_path(const vector<fs::path>& path_list, const fs::path& file_path)
{
    auto path = check_file_in_path(path_list, file_path);
    if (not path)
	throw myexception()<<"Couldn't find file "<<file_path<<" in path '"<<show_path(path_list)<<"'";
    return *path;
}

fs::path find_exe_path(const fs::path& argv0)
{
    /*
      Linux: readlink /proc/self/exe
      FreeBSD: sysctl CTL_KERN KERN_PROC KERN_PROC_PATHNAME -1

      Mac OS X: _NSGetExecutablePath() (man 3 dyld)
      Solaris: getexecname()
      BSD with procfs: readlink /proc/curproc/file
      Windows: GetModuleFileName() with hModule = NULL
    */

    /* For Mac
       char path[1024];
       uint32_t size = sizeof(path);
       if (_NSGetExecutablePath(path, &size) == 0)
       printf("executable path is %s\n", path);
       else
       printf("buffer too small; need size %u\n", size);
    */
    fs::path program_location;

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

