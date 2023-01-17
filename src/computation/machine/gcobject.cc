#include "gcobject.H"


void IntArray::get_regs(std::vector<int>& regs) const
{
    regs = array_regs;
}

void IntArray::update_regs(const std::vector<int>& remap) const
{
    for(auto& r: array_regs)
        r = remap[r];
}

IntArray::IntArray(const std::vector<int>& a)
    :array_regs(a)
{
}

void IntMap::get_regs(std::vector<int>& owned_regs) const
{
    owned_regs.resize(regs.size());
    int i=0;
    for(auto& [key,reg]: regs)
        owned_regs[i++] = reg;
}

void IntMap::update_regs(const std::vector<int>& remap) const
{
    // The problem is that we should like these modifications
    // to affect ALL the shared maps;
    for(auto& [key,reg]: regs)
    {
        if (reg != remap[reg])
        {
            regs = regs.erase(key);
            regs = regs.insert({key,remap[reg]});
        }
    }
}

IntMap::IntMap(const immer::map<int,int>& a)
    :regs(a)
{
}
