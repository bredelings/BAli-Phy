#include "myexception.H"

using std::string;


void test() {
  string error;
  error = string("All alphabets failed:\n") + error;

  myexception e;
  myexception e2(error);
  e<<error;
  throw myexception()<<error;
}
