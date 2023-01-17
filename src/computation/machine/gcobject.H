#ifndef GCOBJECT_H
#define GCOBJECT_H

#include <vector>
#include "object.H"
#include "immer/map.hpp" // for immer::map

class GCObject: public Object
{
public:
    virtual void get_regs(std::vector<int>& regs) const = 0;

    virtual void update_regs(const std::vector<int>& remap) const = 0;

    virtual type_constant type() const {return gc_type;}
};


class IntArray: public GCObject
{
    mutable std::vector<int> array_regs;

public:
    void get_regs(std::vector<int>& regs) const override;
    void update_regs(const std::vector<int>& regs) const override;

    int size() const {return array_regs.size();}
    int get_reg(int i) const {return array_regs[i];}

    IntArray(const std::vector<int>& a);
    IntArray(const IntArray&) = default;
};

class IntMap: public GCObject
{
    mutable immer::map<int,int> regs;

public:
    void get_regs(std::vector<int>& regs) const override;
    void update_regs(const std::vector<int>& regs) const override;

    int size() const {return regs.size();}
    int get_reg(int i) const {return regs.at(i);}
    bool has_reg(int i) const {return regs.count(i);}

    IntMap(const immer::map<int,int>& a);
    IntMap(const IntMap&) = default;
};

#endif
