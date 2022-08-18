#ifndef IO_H
#define IO_H

#include <filesystem>
#include <fstream>
#include <string>
#include <vector>
#include "util/owned-ptr.H"

// this should actually be templatized... <class charT,Alloc,Traits>
std::istream& portable_getline(std::istream&,std::string&);

int num_lines(std::istream& file);

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
    void report_open_error(const std::filesystem::path&, std::ios_base::openmode, bool);

public:

    checked_filebuf* open ( const std::filesystem::path& s, std::ios_base::openmode mode );

    checked_filebuf();
    explicit checked_filebuf(const std::string&);
};

class checked_ifstream: public std::istream
{
    checked_filebuf buf;
public:
    explicit checked_ifstream(const std::filesystem::path&);
    checked_ifstream(const std::filesystem::path&,const std::string&);
};

class checked_ofstream: public std::ostream
{
    checked_filebuf buf;
public:
    explicit checked_ofstream(const std::filesystem::path&, bool=true);
    checked_ofstream(const std::filesystem::path&, const std::string&, bool=true);
};

class istream_or_ifstream: public std::istream
{
    owned_ptr<checked_filebuf> buf;
    nullbuf buf_null;
public:
    void open(std::istream&, const std::string&, const std::filesystem::path&);
    void open(std::istream&, const std::string&, const std::filesystem::path&,const std::string&);

    istream_or_ifstream();
    istream_or_ifstream(std::istream&, const std::string&, const std::filesystem::path&);
    istream_or_ifstream(std::istream&, const std::string&, const std::filesystem::path&, const std::string&);
};

class ostream_or_ofstream: public std::ostream
{
    owned_ptr<checked_filebuf> buf;
    nullbuf buf_null;
public:
    void open(std::ostream&, const std::string&, const std::filesystem::path&);
    void open(std::ostream&, const std::string&, const std::filesystem::path&,const std::string&);

    ostream_or_ofstream();
    ostream_or_ofstream(std::ostream&, const std::string&, const std::filesystem::path&);
    ostream_or_ofstream(std::ostream&, const std::string&, const std::filesystem::path&, const std::string&);
};

class null_ostream: public std::ostream
{
    nullbuf buf;
public:
    null_ostream();
};

std::string read_file(const std::filesystem::path&);

std::string read_file(const std::filesystem::path&, const std::string& description);

std::vector<std::string> read_lines(std::istream& file);
#endif
