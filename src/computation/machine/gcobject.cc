#include "gcobject.H"
#include "util/string/join.H"


void GCObject::get_regs(std::vector<int>& regs) const
{
    regs.clear();
}

void GCObject::update_regs(const std::vector<int>& remap) const
{
}


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

void IntMap::erase(int k)
{
    regs = regs.erase(k);
}

void IntMap::insert(int k, int v)
{
    regs = regs.set(k,v);
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
    // We actually want to modify shared storage here!
    for(auto& [key,reg]: regs)
    {
        if (reg != remap[reg])
        {
            const_cast<int&>(reg) = remap[reg];
            assert(reg);
        }
    }
}

std::string IntMap::print() const
{
    std::vector<std::string> kvs;
    for(auto& [k,r]: regs)
        kvs.push_back(std::to_string(k)+"->" + std::to_string(r));
    return "I{"+join(kvs,",")+"}";
}

IntMap::IntMap(const immer::map<int,int>& a)
    :regs(a)
{
}
