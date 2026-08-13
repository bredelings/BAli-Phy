#ifndef RECORD_FIELD_INFO_H
#define RECORD_FIELD_INFO_H

#include <string>
#include <vector>

#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>

enum class FieldSelectorStatus
{
    SourceVisible,
    SourceHidden
};

struct FieldInfo
{
    std::string name;
    std::string parent_type;
    std::vector<std::string> constructors;
    std::vector<int> positions;
    FieldSelectorStatus selector_status = FieldSelectorStatus::SourceVisible;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(name, parent_type, constructors, positions, selector_status);
    }
};

inline bool source_visible_selector(const FieldInfo& field)
{
    return field.selector_status == FieldSelectorStatus::SourceVisible;
}

#endif
