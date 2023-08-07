#include "message.H"
#include <sstream>
#include "util/text.H"
#include "util/string/join.H"
#include <range/v3/all.hpp>

using std::string;
using std::vector;

namespace views = ranges::views;

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
        col1 = lines[line1-1].size()+1;
    }
    // (line1,col1) == EOF
    if (line2-1 == lines.size() and col2 == 1)
    {
        line2 = lines.size();
        col2 = lines[line2-1].size()+1;
    }
    // Check that the lines exist in the file.
    assert(line1-1 < lines.size());
    assert(line2-1 < lines.size());

    // Check that the columns exist in the file.
    // The end column is the first character AFTER the range.
    assert(col1 -1 <  lines[line1-1].size());
    assert(col2 -1 <= lines[line2-1].size());

    const string& line = lines[line1-1];

    string line_no1 = std::to_string(line1);
    int n = line_no1.size();

    // For multi-line selections, just print the whole first line.
    if (line2 > line1) col2 = line.size();

    out<<string(n+1,' ')<<bold_blue("|")<<"\n";
    out<<bold_blue(line_no1)<<" "<<bold_blue("| ")<<line.substr(0,col1-1)<<bold_red(line.substr(col1-1,col2-col1))<<line.substr(col2-1)<<"\n";
    out<<string(n+1,' ')<<bold_blue("|")<<string(col1,' ')<<bold_red(string(col2-col1,'^'))<<"\n";

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

bool has_errors(const vector<Message>& messages)
{
    for(auto& message: messages)
        if (message.is_error())
            return true;
    return false;
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
