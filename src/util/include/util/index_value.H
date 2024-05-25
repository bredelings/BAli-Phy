#ifndef INDEX_VALUE_H
#define INDEX_VALUE_H

#include <optional>

/// Track *where* we found the max and min.
/// Basically used for getting (max,argmax) and (min,argmin) incrementally.
template <typename T>
struct index_value: std::optional<std::tuple<int,T>>
{
    std::optional<int> index() const
    {
	if (this->has_value())
	    return std::get<0>(this->value());
	else
	    return {};
    }

    std::optional<T> amount() const
    {
	if (this->has_value())
	    return std::get<1>(this->value());
	else
	    return {};
    }

    T check_min(int index, T a) 
	{
	    if (not amount() or (*this->amount() > a))
	    {
		this->operator=(index_value<T>(index,a));
	    }
	    return std::get<1>(this->value());
	}

    T check_max(int index, T a) 
	{
	    if (not amount() or (this->amount() < a))
	    {
		this->operator=(index_value<T>(index,a));
	    }
	    return std::get<1>(this->value());
	}

    index_value() = default;
    index_value(int index, const T& t):std::optional<std::tuple<int,T>>({index,t}) {}
};

#endif
