#ifndef FRESH_VARS_H
#define FRESH_VARS_H

#include <vector>
#include <string>
#include <optional>

#include "computation/expression/var.H" // for var
#include "computation/optimization/occurrence_info.H" // for Core2::Var<>, Occ::Var
#include "computation/haskell/haskell.H" // for Hs::Var, Hs::TypeVar
#include "computation/haskell/coretype.H" // for Hs::Var, Hs::TypeVar

#include "core/ast.H"

class FreshVarState
{
    int var_index = 1;

public:

    bool check_index(int i) const {return i < var_index;}
    int get_index() {return var_index++;}

    void after(int i) { var_index = std::max(var_index, i);}
    void after(const FreshVarState& fv) { after(fv.var_index); }

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar( var_index );
    }
};


class FastString
{
    // Should this be a shared_ptr<string>?
    std::string name_;
    int index_;
public:
    const std::string& short_name() const {return name_;}
    const std::string long_name() const {return name_ +"_"+std::to_string(index_);}

    int index() const {return index_;}

    bool operator<=>(const FastString& fs) { return fs.index() - index();}

    FastString& operator=(const FastString& fs) = default;
    FastString& operator=(FastString&& fs) noexcept = default;

    FastString(const FastString&) = default;
    FastString(FastString&&) noexcept = default;

    FastString(const std::string& n, int i): name_(n),index_(i) {}
};

class FreshVarSource
{
    FreshVarState& state;
    std::optional<std::string> mod_name;

public:
    var get_fresh_var();

    var get_fresh_var(const std::string& name);
    var get_fresh_var(const std::string& name, bool qualified);
    var get_fresh_var(const var& x);
    var get_fresh_var_copy(var x);
    Occ::Var get_fresh_occ_var(const std::string& name);
    Occ::Var get_fresh_var_copy(Occ::Var x);
    var get_fresh_var(const Hs::Var& x);

    std::string qualified_name(const std::string&) const;
    Hs::Var add_mod_name(const Hs::Var&) const;
    Hs::Var get_fresh_Var(const std::string& name, bool qualified);
    Hs::Var get_fresh_Var(const var& x, bool qualified);
    Hs::Var get_fresh_Var(const Hs::Var& x, bool qualified);

    Core2::Var<> get_fresh_core_var(const std::string& name);
    Core2::Var<> get_fresh_core_var(const std::string& name, bool qualified);

    // "Rigid" type vars come from forall-quantified variables.
    // "Meta" type vars are unification type vars.
    MetaTypeVar fresh_meta_type_var(int level, const std::string& name, const Kind& k);
    MetaTypeVar fresh_meta_type_var(int level, const Kind& k);

    TypeVar fresh_rigid_type_var(int level, const std::string& name, const Kind& k);
    TypeVar fresh_rigid_type_var(int level, const Kind& k);

    bool check_index(int i) const;
    int get_index();

    FreshVarSource(FreshVarState& s);
    FreshVarSource(FreshVarState& s, const std::string& mn);
};

var make_var(const Hs::Var& v);

Core2::Var<> make_core_var(const Hs::Var& v);

std::vector<var> make_vars(const std::vector<Hs::Var>& vs);

#endif
