#include <vector>
#include <fstream>

#include <boost/program_options.hpp>
#include <boost/scoped_ptr.hpp>

#include "io.H"
#include "stats-table.H"
#include "myexception.H"
#include "owned-ptr.H"

using namespace std;

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("filenames", value<vector<string> >()->composing(),"Filenames to analyze (empty for STDIN)")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help,h", "Produce help message.")
    ("skip,s",value<int>(),"Number of initial lines to skip.");

  options_description all("All options");
  all.add(invisible).add(visible);

  // positional options
  positional_options_description p;
  p.add("filenames", -1);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);

  if (args.count("help")) {
    cout<<"Usage: stats-cat [OPTIONS] file1 [file2 file3 ... ] \n";
    cout<<"Compute summary statistics for tab-delimited data files.\n\n";
    cout<<visible<<"\n";
    cout<<"Default: Report the median and 95% credible interval for each column.\n\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) 
{ 
  try 
  {
    variables_map args = parse_cmd_line(argc,argv);

    vector<string> filenames = args["filenames"].as< vector<string> >();

    int skip = 0;
    if (args.count("skip"))
      skip = args["skip"].as<int>();

    if (not args.count("filenames"))
      throw myexception()<<"No filenames specified.\n\nTry `"<<argv[0]<<" --help' for more information.";


    // Check that all files have the same field names
    vector<string> field_names;
    vector<boost::shared_ptr<istream> > files(filenames.size());
    for(int i=0;i<filenames.size();i++)
    {
      files[i] = boost::shared_ptr<istream>(new checked_ifstream(filenames[i],"statistics file"));

      if (not *files[i])
	throw myexception()<<"Can't open file '"<<filenames[i]<<"'";

      vector<string> field_names2 = read_header(*files[i]);

      if (i == 0)
	field_names = field_names2;
      else if (field_names != field_names2)
	throw myexception()<<filenames[i]<<": Column names differ from names in '"<<filenames[0]<<"'";
    }

    // Write all the files to cout, in the specified order, but with only one header
    write_header(std::cout,field_names);
    for(int i=0;i<files.size();i++)
    {
      string line;
      for(int line_number=0;portable_getline(*files[i],line);line_number++)
	if (line_number >= skip)
	  std::cout<<line<<"\n";
    }
  }
  catch (std::exception& e) {
    std::cerr<<"stats-cat: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


