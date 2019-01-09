#ifndef INDEX_VALUE_H
#define INDEX_VALUE_H

template <typename T>
struct index_value
{
    int index;
    T value;
    T check_min(int index2, T value2) 
	{
	    if (index == -1 or (value2 < value)) {
		index = index2;
		value = value2;
	    }
	    return value;
	}
    T check_max(int index2, T value2) 
	{
	    if (index == -1 or (value2 > value)) {
		index = index2;
		value = value2;
	    }
	    return value;
	}
    index_value():index(-1),value() {}
    index_value(const T& t):index(-1),value(t) {}
};

#endif
