#ifndef ARGUMENTS_H
#define ARGUMENTS_H

#include <iostream>
#include <string>
#include <map>
#include "util.H"

class Arguments: public std::map<std::string,std::string,lstr> {
public:
  bool set(const std::string&) const;

  void read(int,char *[]);
  void read(std::istream&);

  void parse(const std::string&);
  void add(const std::string&,const std::string&);
  
  void print(std::ostream&);

  template<typename valuetype>  valuetype loadvalue(const std::string& s) {
    if (not set(s))
      std::abort();
    else
      return convertTo<valuetype>((*this)[s]);
  }

  template<typename valuetype>  valuetype loadvalue(const std::string& s,const valuetype& D) {
    if (not set(s)) 
      return D;
    else
      return convertTo<valuetype>((*this)[s]);
  }
};

#endif
