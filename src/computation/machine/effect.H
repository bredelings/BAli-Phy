#ifndef EFFECT_H
#define EFFECT_H
#include "computation/object.H"

class reg_heap;

class effect:public Object
{
public:
    virtual void register_effect(reg_heap&, int r) const = 0;
    virtual void unregister_effect(reg_heap&, int r) const = 0;
};

#endif
