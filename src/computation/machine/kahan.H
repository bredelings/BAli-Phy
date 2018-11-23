#ifndef KAHAN_H
#define KAHAN_H

#include "math/log-double.H"

struct kahan_adder
{
    double value = 0;
    double delta = 0;
    double total_error = 0;
    double unhandled = 0;

    bool operator += (double);
    bool operator -= (double x) {return operator+=(-x);}

    explicit operator double () {return value-delta;}
};

struct kahan_multiplier
{
    kahan_adder data;
    bool operator *=(log_double_t x) {return data += x.log();}
    bool operator /=(log_double_t x) {return data -= x.log();}

    explicit operator log_double_t() {return exp_to<log_double_t>(double(data));}
};

#endif
