#ifndef STARTUP_IO_H
#define STARTUP_IO_H

#include <iostream>
#include <sstream>
#include <string>

struct restore 
{
  std::ostream* stream;
  std::streambuf * old;

  restore(std::ostream& s)
    : stream(&s), old( stream->rdbuf() )
  { }

  ~restore( ) 
  {
    stream->rdbuf( old );
  }
};

/// A stringbuf that write to 2 streambufs
class teebuf: public std::stringbuf
{
protected:
  std::streambuf* sb1;
  std::streambuf* sb2;

public:
  
  int sync() {
    std::string s = str();
    sb1->sputn(s.c_str(), s.length());
    sb2->sputn(s.c_str(), s.length());
    int rc = sb1->pubsync();
    rc = sb2->pubsync();
    str(std::string());
    return rc;
  } 

  std::streambuf* rdbuf1() {return sb1;}
  std::streambuf* rdbuf2() {return sb2;}

  void setbuf1(std::streambuf* sb) {sb1 = sb;}
  void setbuf2(std::streambuf* sb) {sb2 = sb;}

  teebuf(std::streambuf* s1, std::streambuf* s2):
    sb1(s1),
    sb2(s2)
  {}

  ~teebuf() {sync();}
};


#endif
