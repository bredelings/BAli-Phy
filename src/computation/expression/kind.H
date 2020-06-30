#ifndef COMP_EXP_KIND_H
#define COMP_EXP_KIND_H

#include <string>

#include "computation/object.H"
#include "util/owned-ptr.H"

struct kindObj: public Object
{
    virtual kindObj* clone() const = 0;
    virtual ~kindObj() {};

};

typedef owned_ptr<kindObj> kind;

struct kind_star: public kindObj
{
    virtual kind_star* clone() const {return new kind_star();}
    
    virtual bool operator==(const Object& O) const
    {
        if (dynamic_cast<const kind_star*>(&O))
            return true;
        else
            return false;
    }

    std::string print() const {return "*";}
};

struct kind_fun: public kindObj
{
    kind k1;
    kind k2;

    virtual kind_fun* clone() const {return new kind_fun(*this);}
    
    virtual bool operator==(const Object& O) const
    {
        if (auto kO = dynamic_cast<const kind_fun*>(&O))
            return (*k1) == (*kO->k1) and (*k2) == (*kO->k2);
        else
            return false;
    }

    std::string print() const
    {
        auto sk1 = k1->print();
        if (k1.as<kind_fun>())
            sk1 = "("+sk1+")";
        auto sk2 = k2->print();
        return sk1 + "->" + sk2;
    }

    kind_fun(const kind& K1, const kind& K2):k1(K1),k2(K2) {}
};

struct kind_var: public kindObj
{
    std::string name;
    int index;

    virtual kind_var* clone() const {return new kind_var(*this);}
    
    virtual bool operator==(const Object& O) const
    {
        if (auto kO = dynamic_cast<const kind_var*>(&O))
            return (name == kO->name) and (index == kO->index);
        else
            return false;
    }
    std::string print() const {return name;}

    kind_var(const std::string& s, int i=0):name(s),index(i) {}
};

kind kstar();
kind kfun(const kind&, const kind&);
kind kvar(const std::string&, int);

#endif
