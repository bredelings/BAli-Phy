#ifndef TIDY
#define TIDY

#include <set>
#include <map>
#include "computation/haskell/coretype.H"

struct TidyState
{
    std::set<std::string> taken;
    std::map<std::string,int> next_index;
    std::map<std::string,std::string> tidy_names;

    bool is_taken(const std::string& name) const;
    std::string tidy_name(const std::string& name);
};

std::string tidy_print(TidyState& tidy_state, const Type& t);

#endif
