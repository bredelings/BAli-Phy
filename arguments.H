#ifndef ARGUMENTS_H
#define ARGUMENTS_H

#include <iostream>
#include <string>
#include <map>


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
};

#endif
