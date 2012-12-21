#ifndef IO_H
#define IO_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "owned-ptr.H"

// this should actually be templatized... <class charT,Alloc,Traits>
std::istream& portable_getline(std::istream&,std::string&);

template <class T>
struct accumulator
{
  virtual void operator()(const T&)=0;
  virtual void finalize() {}
  virtual ~accumulator() {}
};

template <class T>
struct vector_accumulator: accumulator<T>,
			   public std::vector<T>
{
  void operator()(const T& t){std::vector<T>::push_back(t);}
};

void scan_lines(std::istream& file,int skip,int subsample, int max, 
		accumulator<std::string>& op);

std::vector<std::string> load_lines(std::istream& file,int skip,int subsample, int max);

std::string get_basename(std::string filename);

std::string remove_extension(std::string filename);

// hmm... should not inherit from ifstream, cuz cannot override non virtual open( )

/// A stringbuf that write to 2 streambufs
class nullbuf: public std::streambuf
{
public:
  int overflow(int) { return 0; /*return traits_type::not_eof(c) */ }
};

class checked_filebuf: public std::filebuf
{
  std::string description;
  void report_open_error(const std::string&, std::ios_base::openmode, bool);

public:

  checked_filebuf* open ( const std::string& s, std::ios_base::openmode mode );

  checked_filebuf();
  explicit checked_filebuf(const std::string&);
};

class checked_ifstream: public std::istream
{
  checked_filebuf buf;
public:
  explicit checked_ifstream(const std::string&);
  checked_ifstream(const std::string&,const std::string&);
};

class istream_or_ifstream: public std::istream
{
  owned_ptr<checked_filebuf> buf;
  nullbuf buf_null;
public:
  void open(std::istream&, const std::string&, const std::string&);
  void open(std::istream&, const std::string&, const std::string&,const std::string&);

  istream_or_ifstream();
  istream_or_ifstream(std::istream&, const std::string&, const std::string&);
  istream_or_ifstream(std::istream&, const std::string&, const std::string&, const std::string&);
};

class checked_ofstream: public std::ostream
{
  checked_filebuf buf;
public:
  explicit checked_ofstream(const std::string&,bool=true);
  checked_ofstream(const std::string&,const std::string&,bool=true);
};

class ostream_or_ofstream: public std::ostream
{
  owned_ptr<checked_filebuf> buf;
  nullbuf buf_null;
public:
  void open(std::ostream&, const std::string&, const std::string&);
  void open(std::ostream&, const std::string&, const std::string&,const std::string&);

  ostream_or_ofstream();
  ostream_or_ofstream(std::ostream&, const std::string&, const std::string&);
  ostream_or_ofstream(std::ostream&, const std::string&, const std::string&, const std::string&);
};

class null_ostream: public std::ostream
{
  nullbuf buf;
public:
  null_ostream();
};

std::string read_file(const std::string& filename);

std::string read_file(const std::string& filename, const std::string& description);
#endif
