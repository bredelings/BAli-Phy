#include "mapping.H"

// This is for debugging
std::vector<int> mapping::find_values(int r0) const
{
    std::vector<int> values;
    for(auto& [r,v]: delta_)
        if (r == r0)
            values.push_back(v);
    return values;
}
