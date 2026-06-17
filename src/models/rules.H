#ifndef RULES_H
#define RULES_H

#include <string>
#include <map>
#include <optional>
#include <set>
#include <vector>
#include "models/model-expr.H"
#include "models/model-type.H"
#include <filesystem>

using ParsedType = CM::Type;
using RuleConstraint = CM::Type;

using RuleModelExpr = CM::UntypedExpr;

struct RuleArg
{
    std::string name;
    ParsedType type;
    std::optional<RuleModelExpr> default_value;
    std::optional<RuleModelExpr> alphabet;
    std::optional<std::string> description;
};

struct ComputedRule
{
    std::string name;
    RuleModelExpr value;
};

struct RuleCitationAuthor
{
    std::string name;
};

struct RuleCitationIdentifier
{
    std::string type;
    std::string id;
};

struct RuleCitationLink
{
    std::string url;
    std::optional<std::string> anchor;
};

struct RuleCitation
{
    std::optional<std::string> text;
    std::optional<std::string> title;
    std::optional<std::string> year;
    std::vector<RuleCitationAuthor> authors;
    std::vector<RuleCitationIdentifier> identifiers;
    std::vector<RuleCitationLink> links;
};

struct RuleDocs
{
    std::optional<std::string> title;
    std::optional<std::string> description;
    std::vector<std::string> examples;
    std::vector<std::string> see;
    std::optional<RuleCitation> citation;
    std::vector<std::string> category;
};

struct Rule
{
    std::string name;
    ParsedType result_type;
    std::vector<RuleConstraint> constraints;
    RuleModelExpr call;
    std::vector<RuleArg> args;
    std::set<std::string> imports;
    bool no_log = false;
    bool perform = false;
    std::optional<std::string> extract;
    std::vector<ComputedRule> computed;
    std::vector<std::string> synonyms;
    std::vector<std::string> deprecated_synonyms;
    RuleDocs docs;

    std::optional<std::size_t> arg_index(const std::string&) const;
    const RuleArg* maybe_arg(const std::string&) const;
    RuleArg* maybe_arg(const std::string&);
    const RuleArg& require_arg(const std::string&) const;
    std::string keyword_for_positional_arg(std::size_t) const;
};

const RuleArg& get_arg(const Rule& rule, const std::string& arg_name);
const RuleArg* maybe_get_arg(const Rule& rule, const std::string& arg_name);

CM::Type get_type_for_arg(const Rule& rule, const std::string& arg);

std::string get_keyword_for_positional_arg(const Rule& rule, int i);

class Rules
{
    std::map<std::string,Rule> rules;

    std::map<std::string,std::string> synonyms;

    std::map<std::string,std::string> deprecated_synonyms;

    std::vector<std::filesystem::path> path_list;

public:
    std::optional<Rule> get_rule_for_func(const std::string& s) const;

    const std::map<std::string, Rule>& get_rules() const;

    Rule require_rule_for_func(const std::string& s) const;

    Rules(const std::vector<std::filesystem::path>& path_list);
};

#endif
