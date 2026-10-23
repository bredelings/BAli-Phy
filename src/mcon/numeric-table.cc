#include "mcon/mcon.hh"
#include "table-fields.hh"

#include <algorithm>
#include <stdexcept>
#include <utility>

namespace MCON
{
namespace
{

// Grouping objects represent independent fields, whereas objects/arrays within a value
// retain their original key set/length. Invalidation is permanent: a changed container
// discards all its columns, but a changed child in an unchanged container discards only
// that child. Keys, not insertion order, determine identity. The first retained sample
// defines the candidates because a field absent from it cannot occur in every sample.
struct FieldNode
{
    std::string key;
    std::string name;
    json::kind kind;
    bool group;
    bool varying;
    std::vector<FieldNode> children;
    std::vector<double> values;

    // Cache physical keys and logical names once, checking projected field collisions.
    FieldNode(const json::value& value, std::string name_, bool group_, bool nested,
              std::set<std::string>& logical_fields)
        : name(std::move(name_)), kind(value.is_number() ? json::kind::double_ : value.kind()),
          group(group_), varying(value.is_null())
    {
        if (group && !value.is_object())
            throw std::runtime_error("MCON nested group '"+name+"' must be an object");
        if (auto object = value.if_object())
        {
            for (const auto& [child_key, child_value]: *object)
            {
                bool child_group = group && nested && is_nested_key(child_key);
                std::string child_name = group ? name+std::string(child_key)
                                              : name+"["+std::string(child_key)+"]";
                if (child_group && child_key.ends_with("//")) child_name = name;
                if (group && !child_group && !logical_fields.insert(child_name).second)
                    throw std::runtime_error("Multiple fields project to logical field '"+child_name+"'");
                children.emplace_back(child_value, std::move(child_name), child_group, nested, logical_fields);
                children.back().key = child_key;
            }
        }
        else if (auto array = value.if_array())
            for (std::size_t i = 0; i < array->size(); ++i)
                children.emplace_back((*array)[i], name+"["+std::to_string(i+1)+"]", false, nested, logical_fields);
    }

    // Release numerical storage while preserving the expected keys/lengths of ancestors.
    void invalidate()
    {
        if (varying) return;
        varying = true;
        std::vector<double>().swap(values);
        for (auto& child: children) child.invalidate();
    }

    // Groups tolerate new/missing siblings; structured objects must keep the same keys.
    void append_object(const json::object& object, bool translate_bool, bool store_values)
    {
        if (!group && object.size() != children.size())
        {
            invalidate();
            return;
        }
        for (auto& child: children)
        {
            if (const auto* value = object.if_contains(child.key))
                child.append(*value, translate_bool, store_values);
            else if (group)
                child.invalidate();
            else
            {
                invalidate();
                return;
            }
        }
    }

    // Check shape and append directly as doubles, avoiding JSON/string table intermediates.
    void append(const json::value& value, bool translate_bool, bool store_values)
    {
        if (varying) return;
        if (group && !value.is_object())
            throw std::runtime_error("MCON nested group '"+name+"' must be an object");
        auto value_kind = value.is_number() ? json::kind::double_ : value.kind();
        if (value_kind != kind)
        {
            invalidate();
            return;
        }
        if (auto object = value.if_object())
            append_object(*object, translate_bool, store_values);
        else if (auto array = value.if_array())
        {
            if (array->size() != children.size())
            {
                invalidate();
                return;
            }
            for (std::size_t i = 0; i < array->size(); ++i)
                children[i].append((*array)[i], translate_bool, store_values);
        }
        else if (store_values && value.is_number()) values.push_back(value.to_number<double>());
        else if (store_values && value.is_bool() && translate_bool) values.push_back(value.as_bool());
    }

    // Include stable nonnumeric leaves for naming; report varying containers only once.
    void collect(std::map<std::string, FieldNode*>& stable, std::set<std::string>& variable)
    {
        if (!group && varying)
            variable.insert(name);
        else if (kind == json::kind::object || kind == json::kind::array)
            for (auto& child: children) child.collect(stable, variable);
        else if (!stable.emplace(name, this).second)
            throw std::runtime_error("Multiple values project to table column '"+name+"'");
    }
};

// Initialize on the first retained sample, then reuse the same tree for every record.
void append_sample(std::optional<FieldNode>& root, const json::object& sample,
                   bool nested, bool translate_bool, bool store_values = true)
{
    if (!root)
    {
        std::set<std::string> logical_fields;
        root.emplace(json::value(sample), "", true, nested, logical_fields);
    }
    root->append_object(sample, translate_bool, store_values);
}

// Order and shorten all stable leaf names before selecting numerical columns, as in Log.
NumericTable finish_table(std::optional<FieldNode>& root, std::size_t n_samples,
                          const std::vector<std::string>& specified_fields, bool short_names,
                          bool translate_bool)
{
    NumericTable result;
    result.n_samples = n_samples;
    if (!root) return result;

    std::map<std::string, FieldNode*> stable;
    std::set<std::string> variable;
    root->collect(stable, variable);
    result.varying_fields.assign(variable.begin(), variable.end());
    std::set<std::string> names;
    for (const auto& [name, node]: stable) names.insert(name);
    auto ordered = sort_fields(specified_fields, names);
    auto printed = short_names ? short_fields(ordered) : ordered;
    for (std::size_t i = 0; i < ordered.size(); ++i)
    {
        auto& node = *stable.at(ordered[i]);
        if (node.kind == json::kind::double_ || (translate_bool && node.kind == json::kind::bool_))
            result.columns.emplace_back(std::move(printed[i]), std::move(node.values));
    }
    return result;
}

} // namespace

// Classify JSON-valued tables with the same grouping and shape rules as numerical loading,
// but retain no numerical samples. The caller still owns the input records for TSV output.
std::tuple<std::set<std::string>, std::set<std::string>>
classifyFields(const std::vector<json::object>& samples, bool nested)
{
    std::optional<FieldNode> root;
    for (const auto& sample: samples) append_sample(root, sample, nested, false, false);
    std::map<std::string, FieldNode*> stable;
    std::set<std::string> variable;
    if (root) root->collect(stable, variable);
    std::set<std::string> names;
    for (const auto& [name, node]: stable) names.insert(name);
    return {std::move(names), std::move(variable)};
}

// Apply record limits before parsing samples. Only retained samples affect column stability.
NumericTable read_numeric_table(std::istream& input, int skip, int subsample, int last,
                               bool translate_bool, bool short_names)
{
    if (skip < 0 || subsample < 1)
        throw std::invalid_argument("MCON skip must be nonnegative and subsample must be positive");
    std::string line;
    if (!std::getline(input, line))
        throw std::runtime_error("Missing MCON header");
    auto header = parse(line);
    bool nested = !header.contains("nested") || header.at("nested").as_bool();
    std::vector<std::string> fields;
    if (auto specified = header.if_contains("fields"))
        for (const auto& field: specified->as_array()) fields.emplace_back(field.as_string());

    std::optional<FieldNode> root;
    std::size_t n_samples = 0;
    for (std::size_t record = 0; last < 0 || record < static_cast<std::size_t>(last); ++record)
    {
        if (!std::getline(input, line)) break;
        if (record < static_cast<std::size_t>(skip) || (record-skip) % subsample != 0) continue;
        try
        {
            // Keep the parsed value alive for traversal without copying its root object.
            auto value = json::parse(line, {}, {.allow_infinity_and_nan = true});
            append_sample(root, value.as_object(), nested, translate_bool);
            ++n_samples;
        }
        catch (const std::exception& e)
        {
            throw std::runtime_error("MCON line "+std::to_string(record+2)+": "+e.what());
        }
    }
    if (input.bad()) throw std::runtime_error("Error reading MCON input");
    return finish_table(root, n_samples, fields, short_names, translate_bool);
}

// Feed an existing Log through the same accumulator, preserving this API's diagnostics.
std::vector<std::tuple<std::string, std::vector<double>>>
Log::dump_numeric_table(bool translate_bool, std::optional<bool> short_names) const
{
    std::optional<FieldNode> root;
    for (const auto& sample: samples) append_sample(root, sample, nested, translate_bool);
    auto table = finish_table(root, samples.size(), fields.value_or(std::vector<std::string>{}),
                              short_names.value_or(true), translate_bool);
    if (!table.varying_fields.empty())
    {
        std::cerr<<"varying fields:\n";
        for (const auto& field: table.varying_fields) std::cerr<<"  "<<field<<'\n';
    }
    return std::move(table.columns);
}

} // namespace MCON
