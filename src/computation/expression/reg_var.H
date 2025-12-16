#ifndef REG_VAR_H
#define REG_VAR_H

// A direct reference to a memory location
struct reg_var
{
    int target;
    reg_var(int r):target(r) { }
};

#endif
