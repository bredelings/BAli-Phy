#include "character-property-sample-file.hh"

#include <fstream>
#include <limits>

#include <xxhash.h>

#include "util/myexception.hh"

namespace character_properties
{

namespace
{

/// Release an xxHash streaming state through unique_ptr on every exit path.
struct xxh3_state_deleter
{
    void operator()(XXH3_state_t* state) const {XXH3_freeState(state);}
};

using xxh3_state_ptr = std::unique_ptr<XXH3_state_t, xxh3_state_deleter>;

}

struct sample_file_reader::implementation
{
    std::filesystem::path filename;
    std::ifstream input;
    xxh3_state_ptr hash_state;
    std::uint64_t offset = 0;
    std::uint64_t replay_end = 0;
    bool replaying = false;
    bool finished = false;
    XXH128_hash_t expected_hash{};

    /// Open one binary sample stream and initialize its streaming prefix hash.
    explicit implementation(const std::filesystem::path& filename_)
        : filename(filename_), input(filename, std::ios::binary), hash_state(XXH3_createState())
    {
        if (not input)
            throw myexception()<<"Could not open property sample file '"<<filename.string()<<"'.";
        if (not hash_state or XXH3_128bits_reset(hash_state.get()) == XXH_ERROR)
            throw myexception()<<"Could not initialize the property sample prefix hash.";
    }

    /// Open a fresh stream for replay while retaining the originally captured boundary.
    explicit implementation(const sample_file_snapshot& snapshot)
        : implementation(snapshot.filename)
    {
        replay_end = snapshot.end_offset;
        expected_hash = {snapshot.hash_low, snapshot.hash_high};
        replaying = true;
    }

    /// Add an exact line, including its delimiter, to the byte count and streaming hash.
    void hash_line(const std::string& line)
    {
        constexpr char newline = '\n';
        if (offset == std::numeric_limits<std::uint64_t>::max()
            or line.size() > std::numeric_limits<std::uint64_t>::max() - offset - 1)
            throw myexception()<<filename.string()<<": property sample prefix is too large.";
        if (XXH3_128bits_update(hash_state.get(), line.data(), line.size()) == XXH_ERROR
            or XXH3_128bits_update(hash_state.get(), &newline, 1) == XXH_ERROR)
            throw myexception()<<"Could not update the property sample prefix hash.";
        offset += static_cast<std::uint64_t>(line.size()) + 1;
    }

    /// Read the next newline-committed record without crossing a replay snapshot boundary.
    bool read_line(std::string& line)
    {
        if (finished)
            throw myexception()<<"Property sample reader was used after it was finished.";
        if (replaying and offset == replay_end)
            return false;

        line.clear();
        if (not std::getline(input, line))
        {
            if (input.bad())
                throw myexception()<<"Error while reading property sample file '"<<filename.string()<<"'.";
            if (replaying)
                throw myexception()<<"Property sample file '"<<filename.string()
                                   <<"' is shorter than its captured prefix of "<<replay_end<<" bytes.";
            return false;
        }

        // A newline is the commit marker for a live JSON Lines record. An incomplete tail is outside
        // the snapshot and can be completed by the writer after this pass reaches EOF.
        if (input.eof())
        {
            if (replaying)
                throw myexception()<<"Property sample file '"<<filename.string()
                                   <<"' changed within its captured prefix of "<<replay_end<<" bytes.";
            return false;
        }

        if (replaying and (line.size() >= replay_end - offset))
            throw myexception()<<"Property sample file '"<<filename.string()
                               <<"' changed within its captured prefix of "<<replay_end<<" bytes.";
        hash_line(line);
        return true;
    }

    /// Finalize a capture and return the exact pathname, boundary, and 128-bit digest.
    sample_file_snapshot finish_capture()
    {
        if (finished or replaying)
            throw myexception()<<"Property sample reader is not an unfinished capture.";
        finished = true;
        auto hash = XXH3_128bits_digest(hash_state.get());
        return {filename, offset, hash.low64, hash.high64};
    }

    /// Require replay to have consumed exactly the frozen prefix with the original digest.
    void finish_replay()
    {
        if (finished or not replaying)
            throw myexception()<<"Property sample reader is not an unfinished replay.";
        finished = true;
        if (offset != replay_end)
            throw myexception()<<"Property sample file '"<<filename.string()
                               <<"' is shorter than its captured prefix of "<<replay_end<<" bytes.";
        auto observed_hash = XXH3_128bits_digest(hash_state.get());
        if (not XXH128_isEqual(observed_hash, expected_hash))
            throw myexception()<<"Property sample file '"<<filename.string()
                               <<"' changed within its captured prefix of "<<replay_end<<" bytes.";
    }
};

sample_file_reader::sample_file_reader(const std::filesystem::path& filename)
    : implementation_(std::make_unique<implementation>(filename))
{}

sample_file_reader::sample_file_reader(const sample_file_snapshot& snapshot)
    : implementation_(std::make_unique<implementation>(snapshot))
{}

sample_file_reader::~sample_file_reader() = default;

bool sample_file_reader::read_line(std::string& line)
{
    return implementation_->read_line(line);
}

sample_file_snapshot sample_file_reader::finish_capture()
{
    return implementation_->finish_capture();
}

void sample_file_reader::finish_replay()
{
    implementation_->finish_replay();
}

}
