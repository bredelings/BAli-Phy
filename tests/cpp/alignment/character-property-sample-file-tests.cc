#include "alignment/character-property-sample-file.H"

#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <system_error>
#include <vector>

using character_properties::sample_file_reader;
using character_properties::sample_file_snapshot;

namespace
{

/// Own a uniquely named test directory and remove only that directory when the test exits.
class temporary_directory
{
    std::filesystem::path path_;

public:
    temporary_directory()
    {
        auto base = std::filesystem::temp_directory_path();
        auto stamp = std::chrono::steady_clock::now().time_since_epoch().count();
        for (unsigned attempt = 0; attempt < 100; attempt++)
        {
            path_ = base / ("baliphy-property-snapshot-"+std::to_string(stamp)+"-"+std::to_string(attempt));
            std::error_code error;
            if (std::filesystem::create_directory(path_, error))
                return;
        }
        throw std::runtime_error("Could not create a temporary test directory.");
    }

    ~temporary_directory()
    {
        std::error_code ignored;
        std::filesystem::remove_all(path_, ignored);
    }

    const std::filesystem::path& path() const {return path_;}
};

/// Replace a test file with exactly the requested bytes.
void write_file(const std::filesystem::path& filename, const std::string& contents)
{
    std::ofstream output(filename, std::ios::binary | std::ios::trunc);
    output.write(contents.data(), static_cast<std::streamsize>(contents.size()));
    if (not output)
        throw std::runtime_error("Could not write the sample-file test fixture.");
}

/// Append bytes that simulate records written after the initial summary pass.
void append_file(const std::filesystem::path& filename, const std::string& contents)
{
    std::ofstream output(filename, std::ios::binary | std::ios::app);
    output.write(contents.data(), static_cast<std::streamsize>(contents.size()));
    if (not output)
        throw std::runtime_error("Could not append to the sample-file test fixture.");
}

/// Capture all newline-committed records and return both their contents and frozen prefix.
sample_file_snapshot capture(const std::filesystem::path& filename, std::vector<std::string>& lines)
{
    sample_file_reader input(filename);
    std::string line;
    while (input.read_line(line))
        lines.push_back(line);
    return input.finish_capture();
}

/// Replay one frozen prefix and verify its digest after all committed records are read.
std::vector<std::string> replay(const sample_file_snapshot& snapshot)
{
    sample_file_reader input(snapshot);
    std::vector<std::string> lines;
    std::string line;
    while (input.read_line(line))
        lines.push_back(line);
    input.finish_replay();
    return lines;
}

/// Fail the test with one concise diagnostic instead of depending on a test framework.
void require(bool condition, const std::string& message)
{
    if (condition)
        return;
    std::cerr<<message<<"\n";
    std::exit(1);
}

/// Require replay to reject a pathname whose captured bytes are no longer available.
void require_changed(const sample_file_snapshot& snapshot)
{
    try
    {
        replay(snapshot);
    }
    catch (const std::exception&)
    {
        return;
    }
    require(false, "Replay unexpectedly accepted a changed sample prefix.");
}

}

// Protect the append-only snapshot contract and replacement detection, which command tests cannot
// exercise deterministically between the moments and median passes.
int main()
{
    temporary_directory directory;
    auto filename = directory.path() / "chain.jsonl";

    write_file(filename, "first\nsecond\npartial");
    std::vector<std::string> captured_lines;
    auto snapshot = capture(filename, captured_lines);
    require(captured_lines == std::vector<std::string>({"first", "second"}),
            "Capture did not stop before the unterminated record.");

    append_file(filename, " remainder\nthird\n");
    require(replay(snapshot) == captured_lines, "Replay included records appended after the snapshot.");

    auto replacement = directory.path() / "replacement.jsonl";
    write_file(replacement, "first\nSECOND\nthird\n");
    std::filesystem::remove(filename);
    std::filesystem::rename(replacement, filename);
    require_changed(snapshot);

    write_file(filename, "first\nsecond\nlater\n");
    require(replay(snapshot) == captured_lines, "Replay rejected an identical captured prefix.");

    write_file(filename, "first\n");
    require_changed(snapshot);
}
