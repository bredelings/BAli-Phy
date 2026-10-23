#pragma once

#include <set>
#include <string>
#include <vector>

namespace MCON
{
// Shared ordering for numerical and JSON-valued tables; validates header-listed fields.
std::vector<std::string> sort_fields(const std::vector<std::string>& specified_fields,
                                     const std::set<std::string>& constant_fields);
}
