#ifndef GCOBJECT_H
#define GCOBJECT_H

#include <vector>
#include "object.H"

class GCObject: public Object
{
public:
    virtual void get_regs(std::vector<int>& regs) const = 0;

    virtual void update_regs(const std::vector<int>& remap) const = 0;

    virtual type_constant type() const {return gc_type;}
};


class GCArray: public GCObject
{
    mutable std::vector<int> array_regs;

public:
    void get_regs(std::vector<int>& regs) const override;
    void update_regs(const std::vector<int>& regs) const override;

    int size() const {return array_regs.size();}
    int get_reg(int i) const {return array_regs[i];}

    GCArray(const std::vector<int>& a);
    GCArray(const GCArray&) = default;
};

#endif
