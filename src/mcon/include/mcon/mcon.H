#include <set>
#include <string>
#include <optional>
#include <map>
#include <string_view>
#include <iostream>
#include <boost/json.hpp>
#include <filesystem>

namespace MCON
{
namespace json = boost::json;

bool is_nested_key(std::string_view key);

std::set<std::string> get_keys_non_nested(const json::object&);

std::set<std::string> get_keys_nested(const json::object&);

json::object get_long_names_and_values_data(const std::string&, const json::object&);

json::object atomize(const json::object&, bool nested);

json::object unnest(const json::object&, const std::string& = "");

json::object nest(const json::object&);

std::string chop_nested(const std::string&);

json::object simplify(const json::object&);

std::vector<std::string> short_fields(const std::vector<std::string>& f);

std::ostream& write_tsv_line(std::ostream& o, const std::vector<std::string>& v);

std::string tsv_line(const std::vector<std::string>& v);

std::vector<std::string> tsv_fields(const std::vector<std::string>& first_fields, const json::object& j, bool nested);

std::vector<std::string> get_row(const std::map<std::string,int>& all_fields, const json::object& sample, std::optional<int> sample_index={});

json::object parse(const std::string&);

std::string serialize(const json::value&);

class Log
{
    std::optional<std::vector<std::string>> fields;
    bool nested = true;
    bool atomic = false;
    std::vector<json::object> samples;

public:

    bool is_nested() const {return nested;}

    bool is_atomic() const {return atomic;}

    std::ostream& dump_MCON(std::ostream& o) const;

    std::set<std::string> get_keys() const;

    void unnest();

    void atomize();

    void drop(const std::string&);

    void add_time_field(const std::string& time_field);

    std::vector<Log> split() const;

    std::vector<std::tuple<std::string,std::vector<json::value>>> dump_json_table(std::optional<bool> short_names) const;

    std::vector<std::tuple<std::string,std::vector<double>>> dump_numeric_table(bool translate_bool = true, std::optional<bool> short_names = true) const;

    std::ostream& dump_TSV(std::ostream& o, std::optional<bool> short_names = true) const;

    Log(bool n, bool a, const std::optional<std::vector<std::string>>& f, std::vector<json::object>&& samples);
    Log(const json::object& header);
    Log(const json::object& header, const std::vector<json::object>& samples);
    Log(const json::object& header, std::vector<json::object>&& samples);

    Log(Log&&) noexcept = default;
    Log(const Log&) noexcept = default;

    Log& operator=(const Log&) = default;
    Log& operator=(Log&&) = default;
};

MCON::Log read_logfile(std::istream& input, const std::filesystem::path& filename);

}
