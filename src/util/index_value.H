#ifndef INDEX_VALUE_H
#define INDEX_VALUE_H

#include <optional>

/// Track *where* we found the max and min.
/// Basically used for getting (max,argmax) and (min,argmin) incrementally.
template <typename T>
struct index_value
{
    std::optional<int> index;
    T value;
    T check_min(int index2, T value2) 
	{
	    if (not index or (value2 < value)) {
		index = index2;
		value = value2;
	    }
	    return value;
	}
    T check_max(int index2, T value2) 
	{
	    if (not index or (value2 > value)) {
		index = index2;
		value = value2;
	    }
	    return value;
	}
    index_value() = default;
    index_value(const T& t):value(t) {}
};

#endif
