#include <cassert>

#include <string>
#include <sstream>
#include <iostream>
#include "util.H"
#include "arguments.H"

using std::string;


// assumptions the value of 'key' is increasing

int main(int argc,char* argv[]) {
  Arguments args;
  args.read(argc,argv);

  if (not args.set("key")) 
    throw myexception()<<"argument 'key' not set.";

  if (args.set("size") and args.set("until"))
    throw myexception()<<"cannot set both arguments 'size' and 'until'.";

  bool is_min = args.set("skip");
  double min = args.loadvalue("skip",0.0);

  bool is_max = args.set("size") or args.set("max");
  double max = args.loadvalue<double>("size") + min;

  if (args.set("until"))
    max = args.loadvalue<double>("until");

  if (is_min and is_max and max < min)
    throw myexception()<<"error: maximum value < minimum value";

  string pattern = args["key"] + " = ";

  string line;

  bool in_interval=is_min;
  while(getline(std::cin,line)) {

    // look for the pattern
    int where = line.find(pattern);

    // if no pattern, then
    if (where != -1) {

      // move PAST the pattern
      where += pattern.size();
      double value = convertTo<double>(line.substr(where));
      in_interval = true;
      if (is_min and value < min)
	in_interval = false;
      if (is_max and value > max)
	in_interval = false;

      //    std::cerr<<line<<std::endl;
      //    std::cerr<<"where = "<<where<<std::endl;
      //    std::cerr<<value<<std::endl;
    }

    if (in_interval)
      std::cout<<line<<std::endl;
  }
}
