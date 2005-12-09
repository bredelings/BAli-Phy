#include <iostream>
#include "tree-util.H"
#include "myexception.H"

using boost::program_options::variables_map;
using std::string;
using std::vector;
using std::istream;
using std::cout;
using std::cerr;
using std::endl;

/// Load a tree from command line args "--tree filename"
RootedSequenceTree load_T(const variables_map& args) {
  if (not args.count("tree"))
    throw myexception()<<"Tree file not specified! (--tree <filename>)";
    
  RootedSequenceTree RT;
  RT.read(args["tree"].as<string>());

  return RT;
}

vector<string> load_lines(istream& file,int skip,int subsample, int max)
{
  vector<string> lines;

  string line;
  for(int line_number=0;getline(file,line);line_number++) 
  {
    // don't start if we haven't skipped enough trees
    if (line_number < skip) continue;

    // skip trees unless they are a multiple of 'subsample'
    if ((line_number-skip) % subsample != 0) continue;

    // quit if we've read in 'max' trees
    if (max >= 0 and lines.size() == max) break;

    lines.push_back(line);
  }

  return lines;
}

vector<SequenceTree> load_trees(const vector<string>& lines) 
{
  if (lines.size() == 0)
    throw myexception()<<"No trees were read in!";
  
  vector<SequenceTree> trees;

  for(int i=0;i<lines.size();i++) 
  {
    RootedSequenceTree T;
    try {
      T.parse(lines[i]);
    }
    catch (std::exception& e) {
      cerr<<"Exception: "<<e.what()<<endl;
      cerr<<" Quitting read of tree file"<<endl;
      break;
    }

    trees.push_back(T);
  }

  return trees;
}


vector<SequenceTree> load_trees(istream& file,int skip,int subsample,int max) 
{
  vector<string> lines = load_lines(file,skip,subsample,max);
  return load_trees(lines);
}

