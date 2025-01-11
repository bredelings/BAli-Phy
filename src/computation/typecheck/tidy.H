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
    bool ignore_top_foralls = false;

    bool is_taken(const std::string& name) const;
    std::string tidy_name(const std::string& name);

    std::string print_paren(Type t, bool parenthesize_type_app);

    std::string print(const MetaTypeVar& mtv);
    std::string print(const TypeVar& tv);
    std::string print(const TypeCon& tc);
    std::string print(const TypeApp& app);
    std::string print(const ForallType& forall);
    std::string print(const Context& c);
    std::string print(const ConstrainedType& ct);
    std::string print(const StrictType& sl);
    std::string print(const LazyType& sl);
    std::string print(const Type& name);
};

#endif
