#include "util.H"
#include "paths.H"
#include <vector>

using std::vector;
using std::string;
namespace fs = boost::filesystem;

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
	vector<string> paths = split(PATH,':');
	for(const string& prefix: paths)
	{
	    fs::path p = prefix / argv0;
	    if (fs::exists(p))
		program_location = p;
	}
    }
    program_location = canonical(program_location);
    program_location.remove_filename();

    return program_location;
}

fs::path get_system_lib_path(const string& exe_name)
{
    fs::path system_lib_path = find_exe_path(exe_name);
    if (not system_lib_path.empty())
    {
	system_lib_path.remove_filename();
	system_lib_path = system_lib_path / "lib" / "bali-phy";

	if (not fs::exists(system_lib_path))
	    system_lib_path = "";
    }
    return system_lib_path;
}

fs::path get_user_lib_path()
{
    fs::path user_lib_path;
    if (getenv("HOME"))
    {
	user_lib_path = getenv("HOME");
	user_lib_path = user_lib_path / ".local" / "share" / "bali-phy" / "packages";

	if (not fs::exists(user_lib_path))
	    user_lib_path = "";
    }
    return user_lib_path;
}

