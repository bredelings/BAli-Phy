#ifndef MESSAGE_H
#define MESSAGE_H

#include <string>
#include <vector>
#include <sstream>
#include "computation/parser/located.H"

struct Note
{
    std::string ctx;

    void prepend(const std::string& s) {ctx = s + ctx;}

    std::string print() const {return ctx;}

    template <typename T> Note& operator<<(const T& t)
    {
        std::ostringstream oss;
        oss<<ctx<<t;
        ctx = oss.str();
        return *this;
    }

    Note() = default;
};

struct FileContents
{
    std::string name;
    std::string contents;

    std::string print_range(int line1, int col1, int line2, int col2) const;
};

enum MessageType { ErrorMsg, WarningMsg};

struct Message
{
    // Error or Warning
    MessageType message_type;

    // Should a message have a single location?
    std::optional<yy::location> loc;
    std::vector<Note> notes;

    std::string print(const FileContents& file) const;

    Message(MessageType t, std::optional<yy::location> l, const std::vector<Note>& e);
};

#endif
