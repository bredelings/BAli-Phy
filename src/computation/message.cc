#include "message.H"
#include "util/utf8.H"
#include <algorithm>
#include <sstream>
#include <string_view>
#include "util/text.H"
#include "util/string/join.H"
#include <range/v3/all.hpp>

using std::string;
using std::vector;

namespace views = ranges::views;

// Advance one source column position using the same tab convention as RE/flex:
// UTF-8 scalar values count as one column, and tabs advance to an 8-column stop.
static int column_after_source_char(int column, char32_t code_point)
{
    if (code_point != U'\t')
        return column + 1;

    constexpr int tab_width = 8;
    return column + tab_width - ((column - 1) % tab_width);
}

// Convert Bison's 1-based source columns to byte offsets before slicing a
// UTF-8 line for diagnostics.  Invalid UTF-8 falls back to one byte per column.
static std::size_t byte_offset_for_source_column(std::string_view line, int column)
{
    if (column <= 1)
        return 0;

    std::size_t byte_offset = 0;
    int current_column = 1;
    while(byte_offset < line.size() and current_column < column)
    {
        char32_t code_point = static_cast<unsigned char>(line[byte_offset]);
        std::size_t next_byte = byte_offset + 1;

        if (auto decoded = utf8::decode_next(line, byte_offset))
        {
            code_point = decoded->code_point;
            next_byte = decoded->next_byte;
        }

        int next_column = column_after_source_char(current_column, code_point);
        if (next_column > column)
            break;

        byte_offset = next_byte;
        current_column = next_column;
    }
    return byte_offset;
}

// Compute the source column immediately after a line so multi-line diagnostics
// can highlight the whole first line without treating UTF-8 bytes as columns.
static int source_column_after_line(std::string_view line)
{
    std::size_t byte_offset = 0;
    int column = 1;
    while(byte_offset < line.size())
    {
        char32_t code_point = static_cast<unsigned char>(line[byte_offset]);
        std::size_t next_byte = byte_offset + 1;

        if (auto decoded = utf8::decode_next(line, byte_offset))
        {
            code_point = decoded->code_point;
            next_byte = decoded->next_byte;
        }

        column = column_after_source_char(column, code_point);
        byte_offset = next_byte;
    }
    return column;
}

string FileContents::print_range(int line1, int col1, int line2, int col2) const
{
    std::ostringstream out;

    // Check column and line numbers.
    assert(line1 >= 1);
    assert(col1 >= 1);
    assert(line2 >= 1);
    assert(col2 >= 1);
    assert(line2 >= line1);

    auto lines = get_lines(contents);
    if (lines.empty() and line2 == 1 and col2 == 1)
    {
        return "EMPTY FILE";
    }

    // (line1,col1) == EOF
    if (line1-1 == lines.size() and col1 == 1)
    {
        line1 = lines.size();
        col1 = source_column_after_line(lines[line1-1]);
    }
    // (line2,col2) == EOF
    if (line2-1 == lines.size() and col2 == 1)
    {
        line2 = lines.size();
        col2 = source_column_after_line(lines[line2-1]);
    }
    // Check that the lines exist in the file.
    assert(line1-1 < lines.size());
    assert(line2-1 < lines.size());

    const string& line = lines[line1-1];

    string line_no1 = std::to_string(line1);
    int n = line_no1.size();

    // For multi-line selections, just print the whole first line.
    if (line2 > line1)
        col2 = source_column_after_line(line);

    auto byte_col1 = byte_offset_for_source_column(line, col1);
    auto byte_col2 = byte_offset_for_source_column(line, col2);
    if (byte_col2 < byte_col1)
        byte_col2 = byte_col1;
    int indicator_width = std::max(1, col2 - col1);

    out<<string(n+1,' ')<<bold_blue("|")<<"\n";
    out<<bold_blue(line_no1)<<" "<<bold_blue("| ")<<line.substr(0,byte_col1)<<bold_red(line.substr(byte_col1,byte_col2-byte_col1))<<line.substr(byte_col2)<<"\n";
    out<<string(n+1,' ')<<bold_blue("|")<<string(col1,' ')<<bold_red(string(indicator_width,'^'))<<"\n";

    return out.str();
}


string Message::print(const FileContents& file) const
{
    std::ostringstream out;

    out<<ANSI::bold;
    if (loc)
        out<<(*loc)<<":";
    else
        out<<"<unknown>"<<":";

    out<<((message_type==ErrorMsg)?bold_red(" error:"):" warning:");
    out<<ANSI::bold;
    out<<"\n";

    for(auto& msg: views::reverse(notes))
    {
        auto s = indent_and_wrap(5, terminal_width(), msg.print());
        s = "   • " + s.substr(5);
        out<<s<<"\n";
    }

    if (loc)
    {
        int line1 = loc->begin.line;
        int col1 = loc->begin.column;

        int line2 = loc->end.line;
        int col2 = loc->end.column;

        out<<file.print_range(line1, col1, line2, col2);
    }

    return out.str();
}

bool Message::is_error() const
{
    return message_type == ErrorMsg;
}

bool Message::is_warning() const
{
    return message_type == WarningMsg;
}


Message::Message(MessageType t, std::optional<yy::location> l, const Notes& ns)
    :message_type(t), loc(l), notes(ns)
{
}

bool message_before(const Message& m1, const Message& m2)
{
    if (not m1.loc) return false;
    if (not m2.loc) return true;

    if (m1.loc->begin.line < m2.loc->begin.line) return true;
    if (m1.loc->begin.line > m2.loc->begin.line) return false;

    return (m1.loc->begin.column < m2.loc->begin.column);
}

void show_messages(const FileContents& file, std::ostream& out, vector<Message> messages)
{
    std::sort(messages.begin(), messages.end(), message_before);
    for(auto& msg: messages)
    {
        out<<msg.print(file);
        out<<"\n";
    }

    if (has_errors(messages))
        exit(1);
}

int num_errors(const vector<Message>& messages)
{
    int count = 0;
    for(auto& message: messages)
        if (message.is_error())
            count++;
    return count;
}

bool has_errors(const vector<Message>& messages)
{
    return (num_errors(messages) > 0);
}

void exit_on_error(const vector<Message>& messages, int exit_code)
{
    if (has_errors(messages))
        exit(exit_code);
}

Message error(const std::optional<yy::location>& loc, const Note& note)
{
    return {ErrorMsg, loc, {note}};
}

Message warning(const std::optional<yy::location>& loc, const Note& note)
{
    return {WarningMsg, loc, {note}};
}
