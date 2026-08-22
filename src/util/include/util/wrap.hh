#ifndef WRAP_H
#define WRAP_H

template <typename T>
inline T reflect_less_than(T x,T max)
{
    T delta = x - max;
    if (delta > 0)
	x = max - delta;
    return x;
}

template <typename T>
inline T reflect_more_than(T x,T min)
{
    T delta = x - min;
    if (delta < 0)
	x = min - delta;
    return x;
}

///Reflect x at boundaries 0 and max to get a number in [0,max]
template <typename T>
inline T wrap(T x,T max) {
    if (max == 0)
	return 0;

    // flip around to position x axis
    if (x < 0)
	x = -x;

    // map to [0,2*max)
    int n = (int)(x/(2*max));
    x -= n*2*max;

    if (x > max)
	x = max*2 - x;

    assert(x >= 0 and x <= max);
    return x;
}

/// Reflect x at boundaries min and max to get a number in [min,max]
template <typename T>
inline T wrap(T x, T min, T max) {
    return wrap(x-min,max-min)+min;
}


template <typename T>
inline T minmax(T x,T min, T max) {
    if (x<min)
	return min;
    else if (x>max)
	return max;
    else
	return x;
}

#endif
