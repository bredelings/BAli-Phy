#ifndef SYMBOLS_H
#define SYMBOLS_H

#include <set>
#include <string>
#include <optional>
#include <variant>
#include "computation/haskell/haskell.H"

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
};

struct OpInfo
{
    std::string name;
    fixity_info fixity;
};


struct VarInfo;

// Unknown_symbol is for when we add symbol where the only thing we know is fixity.
// If we create a fixity environment instead, we could avoid that.
enum symbol_type_t {variable_symbol = 0, constructor_symbol, class_method_symbol, default_method_symbol, unknown_symbol};

struct symbol_info
{
    std::string name;
    symbol_type_t symbol_type;
    std::optional<std::string> parent;
    std::optional<int> arity;
    std::optional<fixity_info> fixity;
    Type type;
    std::shared_ptr<VarInfo> info;

    symbol_info(const std::string&, symbol_type_t, const std::optional<std::string>& p, std::optional<int>, std::optional<fixity_info> = {});
};

struct ClassInfo;
struct TypeSynonymInfo;
struct TypeFamInfo;

struct type_info
{
    int category() const;

    struct class_info
    {
        std::set<std::string> methods;
        std::shared_ptr<ClassInfo> info;
    };
    struct data_info
    {
        std::set<std::string> constructors;
        std::set<std::string> fields;
    };
    struct type_syn_info
    {
        std::shared_ptr<TypeSynonymInfo> info;
    };
    struct type_fam_info {
        std::shared_ptr<TypeFamInfo> info;
    };

    bool is_type_other() const; // "->", but now also "~"
    const data_info* is_data() const;
    const class_info* is_class() const;
          class_info* is_class();
    const type_syn_info* is_type_syn() const;
          type_syn_info* is_type_syn();
    const type_fam_info* is_type_fam() const;
          type_fam_info* is_type_fam();

    std::string name;
    // info/children
    std::variant<std::monostate, class_info, data_info, type_syn_info, type_fam_info> info;
    std::optional<fixity_info> fixity; // for the function arrow (->) only
    std::optional<int> arity;

    // Are we using this?
    Kind kind;

};

bool operator==(const symbol_info& S1, const symbol_info& S2);

bool operator==(const type_info& S1, const type_info& S2);

#endif
