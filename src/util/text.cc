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


list<string> get_lines(const string& line)
{
    list<string> lines;

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

vector<string> get_lines_vec(const string& line)
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
optional<list<string>> wrap(string line, int width)
{
    if (line.size() < width) return list<string>{line};

    int loc = line.find_last_of(" \t", width-1);

    if (loc != string::npos)
    {
	if (loc+1 < line.size())
	    return list<string>{line.substr(0,loc),line.substr(loc+1)};
	else
	    return list<string>{line.substr(0,loc)};
    }

    return {};
}

void wrap_and_indent_one_line(int indent, int width, list<string>& lines, list<string>& wrapped_lines)
{
    lines.front() = string(indent,' ') + lines.front();
    if (auto wrap_first = wrap(lines.front(), width))
    {
	// Move wrapped first line into wrapped_lines
	wrapped_lines.push_back(wrap_first->front());
	wrap_first->pop_front();

	// Move remaining lines into wrapped_lines
	lines.pop_front();
	lines.insert(lines.begin(), wrap_first->begin(), wrap_first->end());
    }
    else
    {
	// Move the unwrapped lines into wrapped_lines
	wrapped_lines.push_back(lines.front());
	lines.pop_front();
    }
}

string indent_and_wrap(int indent_first_line, int indent_other_lines, int width, const string& text)
{
    if (text.empty()) return text;

    list<string> wrapped_lines;

    auto lines = get_lines(text);
    assert(not lines.empty());

    // 1. Indent and wrap the first line
    wrap_and_indent_one_line(indent_first_line, width, lines, wrapped_lines);

    while(not lines.empty())
	wrap_and_indent_one_line(indent_other_lines, width, lines, wrapped_lines);

    return join(wrapped_lines, '\n');
}

string indent_and_wrap(int indent, int width, const string& text)
{
    return indent_and_wrap(indent, indent, width, text);
}

string indent(int indent, const string& text)
{
    return indent_and_wrap(indent, 100000, text);
}

const std::string ansi_plain("\033[0m");
const std::string ansi_bold("\033[1m");
const std::string ansi_under("\033[4m");
const std::string ansi_inverse("\033[7m");
const std::string ansi_bold_off("\033[21m");
const std::string ansi_under_off("\033[24m");
const std::string ansi_inverse_off("\033[27m");
const std::string ansi_black("\033[0;30m");
const std::string ansi_bold_black("\033[1;30m");
const std::string ansi_red("\033[0;31m");
const std::string ansi_bold_red("\033[1;31m");
const std::string ansi_green("\033[0;32m");
const std::string ansi_bold_green("\033[1;32m");
const std::string ansi_yellow("\033[1;33m");
const std::string ansi_blue("\033[0;34m");
const std::string ansi_bold_blue("\033[1;34m");
const std::string ansi_magenta("\033[1;35m");
const std::string ansi_cyan("\033[1;36m");

const std::string ansi_bg_grey("\033[1;48;2;180;180;180m");
const std::string ansi_bg_grey2("\033[47;1m");

string red(const string& s)
{
    return ansi_red + s + ansi_plain;
}

string black(const string& s)
{
    return ansi_black + s + ansi_plain;
}

string bold_black(const string& s)
{
    return ansi_bold_black + s + ansi_plain;
}

string bold_red(const string& s)
{
    return ansi_bold_red + s + ansi_plain;
}

string green(const string& s)
{
    return ansi_green + s + ansi_plain;
}

string bold_green(const string& s)
{
    return ansi_bold_green + s + ansi_plain;
}

string blue(const string& s)
{
    return ansi_blue + s + ansi_plain;
}

string bold_blue(const string& s)
{
    return ansi_bold_blue + s + ansi_plain;
}

string cyan(const string& s)
{
    return ansi_cyan + s + ansi_plain;
}

string magenta(const string& s)
{
    return ansi_magenta + s + ansi_plain;
}

string bold(const string& line)
{
    return ansi_bold + line + ansi_plain;
}

// This works
// print u"\u001b[44;1m A \u001b[45;1m B \u001b[46;1m C \u001b[47;1m D \u001b[0m"
string highlight_bg(const string& line)
{
    return ansi_bg_grey2 + line + ansi_plain;
}

string inverse(const string& line)
{
    return ansi_inverse + line + ansi_inverse_off;
}

string underline(const string& line)
{
    return ansi_under + line + ansi_under_off;
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
