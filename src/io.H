#ifndef IO_H
#define IO_H

#include <iostream>
#include <string>
#include <vector>

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
  void operator()(const T& t){push_back(t);}
};

void scan_lines(std::istream& file,int skip,int subsample, int max, 
		accumulator<std::string>& op);

std::vector<std::string> load_lines(std::istream& file,int skip,int subsample, int max);

std::string get_basename(std::string filename);

std::string remove_extension(std::string filename);

#endif
