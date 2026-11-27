#ifndef GCOBJECT_H
#define GCOBJECT_H

#include "computation/haskell/Int.hh"
#include <vector>
#include <optional>
#include "immer/map.hpp" // for immer::map
#include "computation/object.hh"

class GCObject: public Object
{
public:
    virtual void get_regs(std::vector<int>& regs) const;

    virtual void update_regs(const std::vector<int>& remap) const;

    virtual type_constant type() const {return type_constant::gc_type;}
};


class IntArray: public GCObject
{
    mutable std::vector<int> array_regs;

public:
    void get_regs(std::vector<int>& regs) const override;
    void update_regs(const std::vector<int>& regs) const override;

    int size() const {return array_regs.size();}
    int get_reg(int i) const {return array_regs[i];}

    IntArray* clone() const override {return new IntArray(*this);}
    IntArray(const std::vector<int>& a);
    IntArray(const IntArray&) = default;
};

class IntMap: public GCObject
{
    mutable immer::map<haskell_int,int> regs;

public:
    void get_regs(std::vector<int>& regs) const override;
    void update_regs(const std::vector<int>& regs) const override;

    std::size_t size() const {return regs.size();}
    bool has_key(haskell_int i) const {return regs.count(i);}
    // Find the stored interpreter register in one search, without evaluating its value.
    std::optional<int> lookup(haskell_int key) const
    {
        if (auto value = regs.find(key))
            return *value;
        return std::nullopt;
    }
    int operator[](haskell_int i) const {return regs.at(i);}

    void erase(haskell_int k);
    void insert(haskell_int k, int v);

    auto begin() {return regs.begin();}
    auto end() {return regs.end();}

    auto begin() const {return regs.begin();}
    auto end() const {return regs.end();}

    std::string print() const override;
    IntMap* clone() const override {return new IntMap(*this);}

    IntMap() = default;
    IntMap(const IntMap&) = default;
    IntMap(const immer::map<haskell_int,int>& a);
};

#endif
