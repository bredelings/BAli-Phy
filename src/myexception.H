#ifndef MYEXCEPTION_H
#define MYEXCEPTION_H
#include <exception>
#include <string>
#include <sstream>

class myexception: public std::exception {
  std::string why;
public:
  virtual const char* what() const throw() {return why.c_str();}

  template <typename T> myexception& operator<<(const T&);

  myexception() throw() {}
  myexception(const std::string& s) throw() : why(s) {}
  virtual ~myexception() throw() {}
};

template <typename T>
myexception& myexception::operator<<(const T& t) {
  std::ostringstream oss;
  oss<<why<<t;
  why = oss.str();
  return *this;
}

std::string show_stack_trace(int ignore=1);

#endif
