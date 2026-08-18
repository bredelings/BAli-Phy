#ifndef CHARACTER_PROPERTY_SAMPLE_FILE_H
#define CHARACTER_PROPERTY_SAMPLE_FILE_H

#include <cstdint>
#include <filesystem>
#include <memory>
#include <string>

namespace character_properties
{

struct sample_file_snapshot
{
    std::filesystem::path filename;
    std::uint64_t end_offset = 0;
    std::uint64_t hash_low = 0;
    std::uint64_t hash_high = 0;
};

/// Stream complete JSON Lines records while capturing or replaying one immutable file prefix.
class sample_file_reader
{
    struct implementation;
    std::unique_ptr<implementation> implementation_;

public:
    explicit sample_file_reader(const std::filesystem::path& filename);
    explicit sample_file_reader(const sample_file_snapshot& snapshot);
    ~sample_file_reader();

    sample_file_reader(const sample_file_reader&) = delete;
    sample_file_reader& operator=(const sample_file_reader&) = delete;

    bool read_line(std::string& line);
    sample_file_snapshot finish_capture();
    void finish_replay();
};

}

#endif
