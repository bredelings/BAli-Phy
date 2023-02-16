#ifndef SYMBOLS_H
#define SYMBOLS_H

#include <set>
#include <string>
#include <optional>
#include <variant>
#include "computation/haskell/haskell.H"

constexpr auto unknown_fix = Haskell::Fixity::unknown;
constexpr auto left_fix = Haskell::Fixity::infixl;
constexpr auto right_fix = Haskell::Fixity::infixr;
constexpr auto non_fix = Haskell::Fixity::infix;
typedef Haskell::Fixity fixity_t;

// "Fixity is a property of a particular entity (constructor or variable), just like its type;
//  fixity is not a property of that entity’s name."
struct fixity_info
{
    fixity_t fixity = unknown_fix;
    int precedence = -1;
};


struct VarInfo;

enum symbol_type_t {variable_symbol = 0, constructor_symbol = 1, class_method_symbol = 2, unknown_symbol = 3};

struct symbol_info
{
    std::string name;
    symbol_type_t symbol_type;
    std::optional<std::string> parent;
    int arity = -1;
    fixity_info fixity;
    Type type;
    std::shared_ptr<VarInfo> info;

    symbol_info() {};
    symbol_info(const std::string&, symbol_type_t, const std::optional<std::string>& p, int);
    symbol_info(const std::string&, symbol_type_t, const std::optional<std::string>& p, int, fixity_info);
};



struct type_info
{
    int category() const;

    struct class_info
    {
        std::set<std::string> methods;
    };
    struct data_info
    {
        std::set<std::string> constructors;
        std::set<std::string> fields;
    };
    struct type_syn_info {};
    struct type_fam_info {};

    bool is_type_other() const; // "->", but now also "~"
    const class_info* is_class() const;
    const data_info* is_data() const;
    bool is_type_syn() const;
    bool is_type_fam() const;

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
