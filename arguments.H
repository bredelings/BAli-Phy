#ifndef ARGUMENTS_H
#define ARGUMENTS_H

#include <iostream>
#include <string>
#include <map>
#include "util.H"

namespace arguments_local {
  struct lstr {
    bool operator()(const std::string& s1, const std::string& s2) const {
      return strcmp(s1.c_str(), s2.c_str()) < 0;
    }
  };
}

class Arguments: public std::map<std::string,std::string,arguments_local::lstr> {
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
