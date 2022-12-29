#include <optional>
#include <algorithm>
#include "util/text.H"
#include "util/assert.hh"
#include "util/string/join.H"

using std::string;
using std::vector;
using std::list;
using std::optional;

int terminal_width()
{
    int width = 80;
    return width-2;
}

string pad(const string& s, int n)
{
    if (s.size() < n)
	return s+string(n-s.size(),' ');
    return s;
}


vector<string> get_lines(const string& line)
{
    vector<string> lines;

    for(int pos=0; pos<line.size();)
    {
	int next = line.find('\n', pos);
	if (next == string::npos)
	    next = line.size();
	lines.push_back(line.substr(pos,next-pos));
	pos = next + 1;
    }
    return lines;
}

// Given a line containing no line breaks, return the first wrapped line and the rest of the line.
vector<string> wrap_lines(const string& line, int width)
{
    vector<string> lines;

    int pos = 0;
    while (pos < line.size())
    {
        // 1. If the remainder of the line fits, just return that.
        if (pos + width > line.size())
        {
            lines.push_back( line.substr(pos) );
            pos = line.size();
            continue;
        }

        // 2. Try to split at the last space or tab near the edge of the page.
        int loc = pos+width-1;
        for(;loc >= pos; loc--)
            if (line[loc] == ' ' or line[loc] == '\t')
                break;

        // 3. If that doesn't work, split at the first space of tab past the edge of the page.
        if (loc < pos)
            loc = line.find_first_of(" \t", pos + width);

        // 4. If there is no space or tab in the rest of the line, just return the whole thing.
        if (loc == string::npos)
        {
            lines.push_back( line.substr(pos) );
            pos = line.size();
            continue;
        }

        // 5. Check that we actually found a space.
        assert(line[loc] == ' ' or line[loc] == '\t');

        // 6. Otherwise return the first bit, and continue with the rest.
        lines.push_back(line.substr(pos, loc - pos));
        pos = loc + 1;
    }
    
    return lines;
}

vector<string> wrap_lines(const vector<string>& lines, int width)
{
    vector<string> wrapped_lines;
    for(auto& line: lines)
    {
        auto w = wrap_lines(line, width);
        wrapped_lines.insert(wrapped_lines.end(), w.begin(), w.end());
    }
    return wrapped_lines;
}

string indent_and_wrap(int indent_first_line, int indent_other_lines, int width, const string& text)
{
    auto text2 = indent_and_wrap(indent_other_lines, width, text);

    if (indent_first_line < indent_other_lines)
        return text2.substr(indent_other_lines - indent_first_line);
    else if (indent_first_line > indent_other_lines)
        return string(indent_first_line - indent_other_lines,' ') + text2;
    else
        return text2;
}

string indent_and_wrap(int indent, int width, const string& text)
{
    if (text.empty()) return text;

    auto lines = wrap_lines(get_lines(text), width - indent);

    for(auto& line: lines)
        line = string(indent,' ') + line;

    return join(lines,'\n');
}

string indent(int indent, const string& text)
{
    return indent_and_wrap(indent, 100000, text);
}

namespace ANSI
{
    const std::string plain("\033[0m");
    const std::string bold("\033[1m");
    const std::string under("\033[4m");
    const std::string inverse("\033[7m");
    const std::string bold_off("\033[21m");
    const std::string under_off("\033[24m");
    const std::string inverse_off("\033[27m");
    const std::string black("\033[0;30m");
    const std::string bold_black("\033[1;30m");
    const std::string red("\033[0;31m");
    const std::string bold_red("\033[1;31m");
    const std::string green("\033[0;32m");
    const std::string bold_green("\033[1;32m");
    const std::string yellow("\033[1;33m");
    const std::string blue("\033[0;34m");
    const std::string bold_blue("\033[1;34m");
    const std::string magenta("\033[1;35m");
    const std::string cyan("\033[1;36m");

    const std::string bg_grey("\033[1;48;2;180;180;180m");
    const std::string bg_grey2("\033[47;1m");
}

string red(const string& s)
{
    return ANSI::red + s + ANSI::plain;
}

string black(const string& s)
{
    return ANSI::black + s + ANSI::plain;
}

string bold_black(const string& s)
{
    return ANSI::bold_black + s + ANSI::plain;
}

string bold_red(const string& s)
{
    return ANSI::bold_red + s + ANSI::plain;
}

string green(const string& s)
{
    return ANSI::green + s + ANSI::plain;
}

string bold_green(const string& s)
{
    return ANSI::bold_green + s + ANSI::plain;
}

string blue(const string& s)
{
    return ANSI::blue + s + ANSI::plain;
}

string bold_blue(const string& s)
{
    return ANSI::bold_blue + s + ANSI::plain;
}

string cyan(const string& s)
{
    return ANSI::cyan + s + ANSI::plain;
}

string magenta(const string& s)
{
    return ANSI::magenta + s + ANSI::plain;
}

string bold(const string& line)
{
    return ANSI::bold + line + ANSI::plain;
}

// This works
// print u"\u001b[44;1m A \u001b[45;1m B \u001b[46;1m C \u001b[47;1m D \u001b[0m"
string highlight_bg(const string& line)
{
    return ANSI::bg_grey2 + line + ANSI::plain;
}

string inverse(const string& line)
{
    return ANSI::inverse + line + ANSI::inverse_off;
}

string underline(const string& line)
{
    return ANSI::under + line + ANSI::under_off;
}


vector<vector<string>> make_columns(int n, const vector<string>& v)
{
    vector<vector<string>> cols;
    for(int c=0;c<n;c++)
    {
	vector<string> col;
	for(int i=c;i<v.size();i+=n)
	    col.push_back(v[i]);
	cols.push_back(col);
    }
    return cols;
}

int column_width(const vector<string>& column)
{
    int w = 0;
    for(auto& s: column)
	w = std::max<int>(w, s.size());
    return w;
}

int total_width(const vector<vector<string>>& columns)
{
    int w = 0;
    for(auto& column: columns)
	w += column_width(column);
    return w + 3*(columns.size()+1);
}

std::string show_options(const std::vector<std::string>& o)
{
    if (o.empty()) return "";

    auto options = o;
    std::sort(options.begin(), options.end());

    vector<vector<string>> columns = {options};
    for(int i=1;i<=o.size();i++)
    {
	vector<vector<string>> maybe_columns = make_columns(i,options);
	if (total_width(maybe_columns) <= terminal_width())
	    columns = maybe_columns;
	else
	    break;
    }

    vector<int> col_widths;
    for(auto& column: columns)
	col_widths.push_back(column_width(column));

    vector<string> lines;
    for(int i=0;i<columns[0].size();i++)
    {
	string line;
	for(int c=0;c<columns.size();c++)
	{
	    if (i >= columns[c].size()) break;
	    line += "   "+pad(columns[c][i], col_widths[c]);
	}
	lines.push_back(line);
    }
    return join(lines,"\n")+"\n";
}
