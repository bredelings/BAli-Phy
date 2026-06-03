#ifndef UTIL_ORDEREDDOUBLE_H
#define UTIL_ORDEREDDOUBLE_H

#include <cmath>
#include <compare>

struct OrderedDouble
{
    double value = 0.0;

    OrderedDouble() = default;
    OrderedDouble(double x):value(x) {}

    operator double() const {return value;}

    template <class Archive>
    void serialize(Archive& ar) { ar(value); }
};

inline std::strong_ordering operator<=>(OrderedDouble x, OrderedDouble y)
{
    const bool x_nan = std::isnan(x.value);
    const bool y_nan = std::isnan(y.value);

    if (x_nan or y_nan)
    {
        if (x_nan == y_nan) return std::strong_ordering::equivalent;
        return x_nan ? std::strong_ordering::less : std::strong_ordering::greater;
    }

    if (x.value < y.value) return std::strong_ordering::less;
    if (y.value < x.value) return std::strong_ordering::greater;
    return std::strong_ordering::equivalent;
}

inline bool operator==(OrderedDouble x, OrderedDouble y)
{
    return (x <=> y) == 0;
}

#endif
