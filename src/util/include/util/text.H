#ifndef TEXT_H
#define TEXT_H

#include <string>
#include <list>
#include <vector>

int terminal_width();

std::string show_options(const std::vector<std::string>&);

std::string pad(const std::string& s, int n);
std::list<std::string> get_lines(const std::string& line);
std::vector<std::string> get_lines_vec(const std::string& line);

std::string indent_and_wrap(int indent_first_line, int indent_other_lines, int width, const std::string& text);
std::string indent_and_wrap(int indent, int width, const std::string& text);

std::string indent(int indent, const std::string& text);

std::string bold(const std::string& line);
std::string highlight_bg(const std::string& line);
std::string inverse(const std::string& line);
std::string underline(const std::string& line);

std::string black(const std::string& s);
std::string bold_black(const std::string& s);
std::string red(const std::string& s);
std::string bold_red(const std::string& s);
std::string blue(const std::string& s);
std::string bold_blue(const std::string& s);
std::string green(const std::string& s);
std::string bold_green(const std::string& s);
std::string cyan(const std::string& s);
std::string magenta(const std::string& s);
#endif
