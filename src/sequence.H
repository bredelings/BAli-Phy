#ifndef SEQUENCE_H
#define SEQUENCE_H
#include <string>

struct sequence: public std::string 
{
  std::string name;
  std::string comment;

  void strip_gaps();

  sequence() {}

  sequence(const std::string& n,const std::string& c);
};

#endif
