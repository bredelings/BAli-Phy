#include "symbols.H"
#include "util/variant.H"

using std::string;
using std::optional;

symbol_info::symbol_info(const string& s, symbol_type_t st, const optional<string>& p, optional<int> a, optional<fixity_info> f)
    :name(s), symbol_type(st), parent(p), arity(a), fixity(f)
{
    if (a) assert(*a != -1);
}

bool operator==(const symbol_info&S1, const symbol_info& S2)
{
    return (S1.name == S2.name) and (S1.symbol_type == S2.symbol_type) and 
        (S1.arity == S2.arity);
}

bool operator==(const type_info& T1, const type_info& T2)
{
    return (T1.name == T2.name) and (T1.category() == T2.category());
}

int type_info::category() const
{
    return info.index();
}

bool type_info::is_type_other() const
{
    return (bool)to<std::monostate>(info);
}

const type_info::class_info* type_info::is_class() const
{
    return to<class_info>(info);
}

type_info::class_info* type_info::is_class()
{
    return to<class_info>(info);
}

const type_info::data_info* type_info::is_data() const
{
    return to<data_info>(info);
}

const type_info::type_syn_info* type_info::is_type_syn() const
{
    return to<type_syn_info>(info);
}

type_info::type_syn_info* type_info::is_type_syn()
{
    return to<type_syn_info>(info);
}

const type_info::type_fam_info* type_info::is_type_fam() const
{
    return to<type_fam_info>(info);
}

type_info::type_fam_info* type_info::is_type_fam()
{
    return to<type_fam_info>(info);
}

const type_info::data_fam_info* type_info::is_data_fam() const
{
    return to<data_fam_info>(info);
}

type_info::data_fam_info* type_info::is_data_fam()
{
    return to<data_fam_info>(info);
}

