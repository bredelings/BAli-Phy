#pragma once

#include <boost/json.hpp>
#include <tuple>
#include <set>
#include <string>
#include <vector>

namespace MCON
{
// Shared structural classification; nested keys denote groups only in nested MCON.
std::tuple<std::set<std::string>, std::set<std::string>>
classifyFields(const std::vector<boost::json::object>& samples, bool nested);

// Shared ordering for numerical and JSON-valued tables; validates header-listed fields.
std::vector<std::string> sort_fields(const std::vector<std::string>& specified_fields,
                                     const std::set<std::string>& constant_fields);
}
