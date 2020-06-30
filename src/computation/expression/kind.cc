#include "kind.H"

kind kstar() {return kind_star();}

kind kfun(const kind& k1, const kind& k2)
{
    return new kind_fun(k1->clone(), k2->clone());
}

kind kvar(const std::string& name, int index)
{
    return new kind_var(name,index);
}
