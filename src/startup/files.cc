#include <boost/filesystem/operations.hpp>
#include <iostream>

#include "files.H"
#include "util.H"
#include "myexception.H"
#include "version.H"
#include "computation/module.H"
#include "computation/loader.H"

using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::ofstream;
using std::ostream;
using std::shared_ptr;

namespace fs = boost::filesystem;
namespace po = boost::program_options;
using po::variables_map;

/// Close the files.
void close_files(vector<shared_ptr<ofstream>>& files)
{
    for(int i=0;i<files.size();i++)
	files[i]->close();
    files.clear();
}

/// Delete the files specified by 'filenames'
void delete_files(vector<string>& filenames)
{
    for(int i=0;i<filenames.size();i++)
	fs::remove(filenames[i]);
    filenames.clear();
}

vector<shared_ptr<ofstream>> open_files(int proc_id, const string& name, vector<string>& names)
{
    vector<shared_ptr<ofstream>> files;
    vector<string> filenames;

    for(int j=0;j<names.size();j++) 
    {
	string filename = name + "C" + convertToString(proc_id+1)+"."+names[j];
      
	if (fs::exists(filename)) {
	    close_files(files);
	    delete_files(filenames);
	    throw myexception()<<"Trying to open '"<<filename<<"' but it already exists!";
	}
	else {
	    files.push_back(shared_ptr<ofstream>(new ofstream(filename.c_str())));
	    filenames.push_back(filename);
	}
    }

    names = filenames;

    return files;
}

string open_dir(const string& dirbase)
{
    // FIXME. Maybe the ability to create arbitrary files should not be allowed?

    // 1. Create the parent directory.
    auto parent = fs::path(dirbase).parent_path();
    if (not parent.empty())
	fs::create_directories(parent);

    // 2. Try to create the child directory
    for(int i=1;;i++)
    {
	string dirname = dirbase + "-" + convertToString(i);

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
	vector<string> alignment_filenames = args["align"].as<vector<string> >();
	for(int i=0;i<alignment_filenames.size();i++)
	    alignment_filenames[i] = remove_extension( fs::path( alignment_filenames[i] ).leaf().string() );
	name = join(alignment_filenames,'-');
    }
    else if (args.count("model"))
    {
	string filename = args["model"].as<string>();
	Module M ( module_loader({}).read_module_from_file(filename) );
	name = M.name;
	name = get_unqualified_name(name);
    }
    else if (args.count("Model"))
    {
	name = args["Model"].as<string>();
	name = get_unqualified_name(name);
    }

    return name;
}

/// Create the directory for output files and return the name
string init_dir(const variables_map& args)
{
    string name = run_name(args);
    
    string dirname = open_dir(name);
    cerr<<"Created directory '"<<dirname<<"/' for output files."<<endl;
    return dirname;
}

#if defined _MSC_VER || defined __MINGW32__
#include <windows.h>
#include <errno.h>
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

void run_info(json& info, int /*proc_id*/, int argc, char* argv[])
{
    json command;
    for(int i=0;i<argc;i++)
	command.push_back(argv[i]);

    time_t now = time(NULL);
    string start_time = ctime(&now);

    json env = {};
    for(auto& var: {"SLURM_JOBID", "JOB_ID", "LSB_JOBID"})
	if (auto evar = getenv(var))
	    env[var] = evar;

    info["command"] = command;
    info["directory"] = fs::initial_path().string();
    info["start time"] = start_time;
    info["environment"] = env;
    info["pid"] = getpid();
    info["hostname"] = hostname();
    json program = version_info();
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

/// Create output files for thread 'proc_id' in directory 'dirname'
vector<shared_ptr<ostream>> init_files(int proc_id, const string& dirname,
				       int argc,char* argv[])
{
    vector<shared_ptr<ostream>> files;

    vector<string> filenames;
    filenames.push_back("out");
    filenames.push_back("err");
    filenames.push_back("run.json");

    vector<shared_ptr<ofstream>> files2 = open_files(proc_id, dirname+"/",filenames);
    files.clear();
    for(int i=0;i<files2.size();i++)
	files.push_back(files2[i]);

    ostream& s_out = *files[0];
    
    s_out<<"command: ";
    for(int i=0;i<argc;i++) {
	s_out<<quote_string(argv[i]);
	if (i != argc-1) s_out<<" ";
    }
    s_out<<endl;
    {
	time_t now = time(NULL);
	s_out<<"start time: "<<ctime(&now)<<endl;
    }
    print_version_info(s_out);
    s_out<<"directory: "<<fs::initial_path().string()<<endl;
    s_out<<"subdirectory: "<<dirname<<endl;
    if (getenv("SLURM_JOBID"))
	s_out<<"SLURM_JOBID: "<<getenv("SLURM_JOBID")<<endl;
    if (getenv("JOB_ID"))
	s_out<<"JOB_ID: "<<getenv("JOB_ID")<<endl;
    if (getenv("LSB_JOBID"))
	s_out<<"LSB_JOBID: "<<getenv("LSB_JOBID")<<endl;
    s_out<<"hostname: "<<hostname()<<endl;
    s_out<<"PID: "<<getpid()<<endl;
#ifdef HAVE_MPI
    mpi::communicator world;
    s_out<<"MPI_RANK: "<<world.rank()<<endl;
    s_out<<"MPI_SIZE: "<<world.size()<<endl;
#endif
    s_out<<endl;

    //  files[0]->precision(10);
    //  cerr.precision(10);

    return files;
}

