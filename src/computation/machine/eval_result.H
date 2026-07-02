#ifndef EVAL_RESULT_H
#define EVAL_RESULT_H

// Result from entering a register: dep_reg is the evaluated register used for
// dependency/call bookkeeping, while value_reg holds the WHNF value.
struct EvalResult
{
    int dep_reg = 0;
    int value_reg = 0;
};

#endif
