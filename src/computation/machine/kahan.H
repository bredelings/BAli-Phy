#ifndef KAHAN_H
#define KAHAN_H

#include "util/math/log-double.H"

struct kahan_adder
{
    double value = 0;
    double delta = 0;
    double total_error = 0;
    double unhandled = 0;

    void reset_unhandled()
    {
	unhandled = 0;
    }

    void reset()
    {
	value = 0;
	delta = 0;
	total_error = 0;
	unhandled = 0;
    }

    bool operator += (double);
    bool operator -= (double x) {return operator+=(-x);}

    explicit operator double () {return value-delta+unhandled;}
};

struct kahan_multiplier
{
    kahan_adder data;
    bool operator *=(log_double_t x) {return data += x.log();}
    bool operator /=(log_double_t x) {return data -= x.log();}

    void reset_unhandled()
    {
	data.reset_unhandled();
    }

    void reset() {data.reset();}

    explicit operator log_double_t() {return exp_to<log_double_t>(double(data));}
};

#endif
