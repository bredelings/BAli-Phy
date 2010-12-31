#ifndef VALUE_HC
#define VALUE_HC
#include "value.H"

template <typename T, typename U>
void ConversionValue<T,U>::update(const Values& V, const std::vector<int>& mapping)
{
  U args1 = V.get_value_as<U>(mapping[0]);
  T t ( args1 );
  ValueBase::data = t;
  ValueBase::up_to_date=true;
}

#endif
