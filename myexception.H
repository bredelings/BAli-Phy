#ifndef MYEXCEPTION_H
#define MYEXCEPTION_H
#include <exception>
#include <string>
using std::string;

class myexception: public std::exception {
  string why;
public:
  virtual const char* what() const throw() {return why.c_str();}

  myexception(const string& s) throw() : why(s) {}
  virtual ~myexception() throw() {}
};

#endif
