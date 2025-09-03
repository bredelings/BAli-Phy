#ifndef SYMBOLS_H
#define SYMBOLS_H

#include <set>
#include <string>
#include <optional>
#include <variant>
#include "computation/haskell/haskell.H"
#include "computation/unfolding.H"

#include <cereal/types/memory.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/variant.hpp>
#include <cereal/types/optional.hpp>
#include <cereal/types/set.hpp>

constexpr auto left_fix = Haskell::Fixity::infixl;
constexpr auto right_fix = Haskell::Fixity::infixr;
constexpr auto non_fix = Haskell::Fixity::infix;
typedef Haskell::Fixity fixity_t;

// "Fixity is a property of a particular entity (constructor or variable), just like its type;
//  fixity is not a property of that entity’s name."
struct fixity_info
{
    fixity_t fixity;
    int precedence;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(fixity,precedence);
    }
};

struct OpInfo
{
    std::string name;
    fixity_info fixity;
};


struct DataConInfo;
struct InstanceInfo;
struct EqInstanceInfo;

// Unknown_symbol is for when we add symbol where the only thing we know is fixity.
// If we create a fixity environment instead, we could avoid that.
enum class symbol_type_t
{
    unknown,
    variable,
    constructor,
    superclass_selector,
    class_method,
    default_method,
    instance_method,
    instance_superclass_selector,
    instance_dfun,
};

struct symbol_info
{
    std::string name;
    symbol_type_t symbol_type;
    std::optional<std::string> parent;
    std::optional<int> arity;
    std::optional<fixity_info> fixity;
    Type type;
    Unfolding unfolding;
    std::shared_ptr<DataConInfo> con_info;
    std::shared_ptr<InstanceInfo> instance_info;
    std::shared_ptr<EqInstanceInfo> eq_instance_info;

    // Set to false if this is just for exporting information about vars like Data.Bool.$v#55 that are created by optimization.
    // Variables like this currently have no type info.
    bool visible = true;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(name, symbol_type, parent, arity, fixity, type, unfolding, con_info, instance_info, eq_instance_info, visible);
    }

    symbol_info(const std::string&, symbol_type_t, const std::optional<std::string>& p, std::optional<int>, std::optional<fixity_info> = {});

private:
    friend cereal::access;

    symbol_info() = default;
};

struct ClassInfo;
struct TypeSynonymInfo;
struct TypeFamInfo;
struct DataFamInfo;

struct type_info
{
    int category() const;

    struct class_info
    {
        std::set<std::string> methods;
        std::shared_ptr<ClassInfo> info;

	template <class Archive> void serialize(Archive& ar) { ar(methods, info);}
    };
    struct data_info
    {
        std::set<std::string> constructors;
        std::set<std::string> fields;

	template <class Archive> void serialize(Archive& ar) { ar(constructors, fields);}
    };
    struct type_syn_info
    {
        std::shared_ptr<TypeSynonymInfo> info;

	template <class Archive> void serialize(Archive& ar) { ar(info);}
    };
    struct type_fam_info {
        std::shared_ptr<TypeFamInfo> info;

	template <class Archive> void serialize(Archive& ar) { ar(info);}
    };
    struct data_fam_info {
	std::shared_ptr<DataFamInfo> info;

	template <class Archive> void serialize(Archive& ar) { ar(info);}
    };

    bool is_type_other() const; // "->", but now also "~"
    const data_info* is_data() const;
    const class_info* is_class() const;
          class_info* is_class();
    const type_syn_info* is_type_syn() const;
          type_syn_info* is_type_syn();
    const type_fam_info* is_type_fam() const;
          type_fam_info* is_type_fam();
    const data_fam_info* is_data_fam() const;
          data_fam_info* is_data_fam();

    std::string name;
    // info/children
    std::variant<std::monostate, class_info, data_info, type_syn_info, type_fam_info, data_fam_info> info;
    std::optional<fixity_info> fixity; // for the function arrow (->) only
    std::optional<int> arity;

    // Are we using this?
    Kind kind;

    template <class Archive> void serialize(Archive& ar) { ar(name,info,fixity,arity,kind); }
};

bool operator==(const symbol_info& S1, const symbol_info& S2);

bool operator==(const type_info& S1, const type_info& S2);

#endif
