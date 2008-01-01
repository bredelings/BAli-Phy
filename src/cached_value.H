#ifndef CACHED_VALUE_H
#define CACHED_VALUE_H

template <typename T>
class cached_value 
{
  T value_;
  bool valid;
public:
  bool is_valid() const {return valid;}
  void invalidate() {valid=false;}
  void validate() {valid=true;}

  const T& value() const {return value_;}
        T& value()       {return value_;}

  cached_value& operator=(const T& t) {value_ = t;valid=true;return *this;}

  operator const T&() const {return value();}
  operator       T&()       {return value();}

  cached_value():valid(false) {}
  cached_value(const T& t):value_(t),valid(true) {}
};

#endif
