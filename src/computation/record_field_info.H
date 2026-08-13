#ifndef RECORD_FIELD_INFO_H
#define RECORD_FIELD_INFO_H

#include <string>
#include <vector>

#include "computation/core/type.H"

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
    Type field_type;
    FieldSelectorStatus selector_status = FieldSelectorStatus::SourceVisible;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(name, parent_type, constructors, positions, field_type, selector_status);
    }
};

inline bool source_visible_selector(const FieldInfo& field)
{
    return field.selector_status == FieldSelectorStatus::SourceVisible;
}

#endif
