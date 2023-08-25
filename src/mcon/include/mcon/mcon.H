#include <set>
#include <string>
#include <optional>
#include <map>
#include <string_view>
#include <iostream>
#include "nlohmann/json.hpp"

namespace MCON
{
using nlohmann::json;

bool is_nested_key(std::string_view key);

std::set<std::string> get_keys_non_nested(const json&);

std::set<std::string> get_keys_nested(const json&);

json get_long_names_and_values_data(const std::string&, const json&);

json atomize(const json&, bool nested);

json unnest(const json&, const std::string& = "");

json nest(const json&);

std::string_view chop_nested(std::string_view);

json simplify(const json&);

std::vector<std::string> short_fields(const std::vector<std::string>& f);

std::ostream& write_tsv_line(std::ostream& o, const std::vector<std::string>& v);

std::string tsv_line(const std::vector<std::string>& v);

std::vector<std::string> get_row(const std::map<std::string,int>& all_fields, const json& sample, std::optional<int> sample_index={});

class Log
{
    std::optional<std::vector<std::string>> fields;
    bool nested = true;
    bool atomic = false;
    std::vector<json> samples;
public:

    bool is_nested() const {return nested;}

    bool is_atomic() const {return atomic;}

    std::ostream& dump_MCON(std::ostream& o) const;

    std::set<std::string> get_keys() const;

    void unnest();

    void atomize();

    std::ostream& dump_TSV(std::ostream& o, std::optional<bool> short_names = {}) const;

    Log(const json& header);
    Log(const json& header, const std::vector<json>& samples);
    Log(const json& header, std::vector<json>&& samples);
};

MCON::Log read_logfile(std::istream& input, const std::string& filename);

}
