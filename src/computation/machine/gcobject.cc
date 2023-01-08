#include "gcobject.H"


void GCArray::get_regs(std::vector<int>& regs) const
{
    regs = array_regs;
}

void GCArray::update_regs(const std::vector<int>& remap) const
{
    for(auto& r: array_regs)
        r = remap[r];
}

GCArray::GCArray(const std::vector<int>& a)
    :array_regs(a)
{
}
