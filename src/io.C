#include "io.H"

#include <boost/filesystem/operations.hpp>
#include "myexception.H"

using namespace std;

namespace fs = boost::filesystem;


/// \brief Read a line from a file with their UNIX or DOS or Mac line endings.
///
/// \param file The input stream
/// \param s The line that was read.
/// 
istream& portable_getline(istream& file,string& s)
{
  const char CR = 13;
  const char LF = 10;

  s.clear();

  int c;
  while (file.good())
  {
    c = file.get();

    // we just read an EOF
    if (file.eof()) break;

    // we just read an EOL
    if (c == CR or c == LF) break;

    s.append(1,c);
  }

  if (!file.good()) return file;

  // If the EOL character is a CR, then also skip any following LF
  if (c == CR and file.peek() == LF)
    file.ignore();

  return file;
}


void scan_lines(istream& file,int skip,int subsample, int max, 
		accumulator<string>& op)
{
  int n_lines=0;
  string line;
  for(int line_number=0;portable_getline(file,line);line_number++) 
  {
    // don't start if we haven't skipped enough trees
    if (line_number < skip) continue;

    // skip trees unless they are a multiple of 'subsample'
    if ((line_number-skip) % subsample != 0) continue;

    // quit if we've read in 'max' trees
    if (max >= 0 and n_lines == max) break;

    // should this be protected by a try { } catch(...) {} block?
    op(line);
    n_lines++;
  }
}

vector<string> load_lines(istream& file,int skip,int subsample, int max)
{
  vector_accumulator<string> lines;

  scan_lines(file,skip,subsample,max,lines);

  return lines;
}

/// \brief Get the basename of a filename (i.e. remove parent directories.)
///
/// \param filename The filename.
///
string get_basename(string filename)
{
  // remove the pathname 
  while(filename.find('/') != -1) 
    filename = filename.substr(filename.find('/')+1);

  return filename;
}

/// \brief Remove the extension from a filename
///
/// \param filename The filename.
///
string remove_extension(string filename)
{
  // remove the extension
  int dot = filename.rfind('.');
  string name = filename;
  if (dot != -1)
    name = filename.substr(0,dot);
  return name;
}

void checked_ifstream::check(const string& description)
{
  if (not good()) {
    close();
    myexception e;
    e<<"Trying to open "<<description<<" '"<<filename<<"'";
    if (not fs::exists(filename))
      e<<": file does not exist.";
    else
      e<<": file exists, but can't be opened.";
    throw e;
  }
}

checked_ifstream::checked_ifstream(const string& s)
  :ifstream(s.c_str()),filename(s)
{
  check("file");
}

checked_ifstream::checked_ifstream(const string& s, const string& description)
  :ifstream(s.c_str()),filename(s)
{
  check(description);
}

