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

  // Make sure that while(getline()) terminates:
  //  - read at least one char in order to set failbit on an empty file.
  int c = file.get();

  do
  {
    // we just read an EOF
    if (file.eof()) break;

    // we just read an EOL
    if (c == CR or c == LF) break;

    s.append(1,c);

    c = file.get();

  } while(file);

  // If the EOL character is a CR, then also skip any following LF
  if (c == CR and file.good() and file.peek() == LF)
    file.ignore();

  // FIXME: redo this using the underlying buffer...
  // If we read to the EOF, then the last file.get() will set the failbit, because
  //  it didn't return a character.  But we don't want this, unless s is empty.
  if (file.eof() and s.size() and file.fail())
    file.clear( file.rdstate() & ~ios::failbit );

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

void checked_filebuf::report_open_error(const string& filename, ios_base::openmode mode, bool existed_before)
{
  myexception e;
  bool exists_now = fs::exists(filename);

  close();

  if ((mode&ios_base::out) and not (mode&ios_base::in))
    e<<"Failed to write to "<<description<<" '"<<filename<<"'";
  else if ((mode&ios_base::in) and not (mode&ios_base::out))
    e<<"Failed to read from "<<description<<" '"<<filename<<"'";
  else
    e<<"Failed to open "<<description<<" '"<<filename<<"'";

  if (mode&ios_base::out)
  {
    if (existed_before and not (mode|ios_base::trunc))
      e<<": file exists, refusing to over-write.";
    if (existed_before and mode|ios_base::trunc)
      e<<": file exists, do you have permission to overwrite?";
    else if (not existed_before and exists_now)
      e<<": file created, but cannot be written to.";
    else if (not existed_before and not exists_now)
      e<<": perhaps you don't have permission to create a file here?";
  }
  else if (mode&ios_base::in)
  {
    if (not fs::exists(filename))
      e<<": file does not exist.";
    else
      e<<": file exists, but can't be opened.";
  }

  throw e;
}

checked_filebuf * checked_filebuf::open ( const std::string& filename, std::ios_base::openmode mode )
{
  bool already_existed = fs::exists(filename);
  std::filebuf* buf = 0;

  // open the file if either we're not going to overwrite it, or we're opening the truncate flag
  if (!already_existed or not (mode&ios_base::out) or (mode&ios_base::trunc))
      buf = std::filebuf::open(filename.c_str(), mode);

  if (!buf)
    report_open_error(filename, mode, already_existed);

  return this;
}

checked_filebuf::checked_filebuf()
  :description("file")
{
}

checked_filebuf::checked_filebuf(const string& s)
  :description(s)
{
}

checked_ifstream::checked_ifstream(const string& filename)
  :buf("file")
{
  this->init(&buf);
  buf.open(filename, ios_base::in);
}

checked_ifstream::checked_ifstream(const string& filename, const string& description)
  :buf(description)
{
  this->init(&buf);
  buf.open(filename, ios_base::in);
}

void istream_or_ifstream::open(std::istream& is, const std::string& is_name, const std::string& filename, const std::string& description)
{
  if (buf)
    throw myexception()<<"Cannot reopen file!\n";

  if (filename == is_name)
    this->init(is.rdbuf());
  else
  {
    buf = claim(new checked_filebuf(description));
    this->init(buf.get());
    buf->open(filename, ios_base::in);
  }
}

istream_or_ifstream::istream_or_ifstream()
{
  this->init(&buf_null);
}

istream_or_ifstream::istream_or_ifstream(std::istream& is, const std::string& is_name, const std::string& filename)
{
  open(is,is_name,filename,"file");
}

istream_or_ifstream::istream_or_ifstream(std::istream& is, const std::string& is_name, const std::string& filename,
					 const std::string& description)
{
  open(is,is_name,filename,description);
}


checked_ofstream::checked_ofstream(const string& filename,bool trunc)
  :buf("file")
{
  this->init(&buf);
  std::ios_base::openmode flags = ios_base::out;
  if (trunc)
    flags |= ios_base::trunc;

  buf.open(filename, flags);
}

checked_ofstream::checked_ofstream(const string& filename, const string& description, bool trunc)
  :buf(description)
{
  this->init(&buf);
  std::ios_base::openmode flags = ios_base::out;
  if (trunc)
    flags |= ios_base::trunc;

  buf.open(filename, flags);
}

void ostream_or_ofstream::open(std::ostream& os, const std::string& os_name, const std::string& filename, const std::string& description)
{
  if (buf)
    throw myexception()<<"Cannot reopen file!\n";

  if (filename == os_name)
    this->init(os.rdbuf());
  else
  {
    buf = claim(new checked_filebuf(description));
    this->init(buf.get());
    buf->open(filename, ios_base::out|ios_base::trunc);
  }
}

ostream_or_ofstream::ostream_or_ofstream()
{
  this->init(&buf_null);
}

ostream_or_ofstream::ostream_or_ofstream(std::ostream& os, const std::string& os_name, const std::string& filename)
{
  open(os,os_name,filename,"file");
}

ostream_or_ofstream::ostream_or_ofstream(std::ostream& os, const std::string& os_name, const std::string& filename,
					 const std::string& description)
{
  open(os,os_name,filename,description);
}

null_ostream::null_ostream()
  :ostream(&buf)
{ }

string read_file(const string& filename)
{
  checked_ifstream file(filename);
  std::stringstream buffer;
  buffer << file.rdbuf();
  return buffer.str();
}

string read_file(const string& filename, const string& description)
{
  checked_ifstream file(filename,description);
  std::stringstream buffer;
  buffer << file.rdbuf();
  return buffer.str();
}

