#ifndef TEXT_H
#define TEXT_H

#include <string>
#include <list>

int terminal_width();

std::string pad(const std::string& s, int n);
std::list<std::string> get_lines(const std::string& line);

std::string indent_and_wrap(int indent_first_line, int indent_other_lines, int width, const std::string& text);
std::string indent_and_wrap(int indent, int width, const std::string& text);

std::string indent(int indent, const std::string& text);

std::string bold(const std::string& line);
std::string highlight_bg(const std::string& line);
std::string inverse(const std::string& line);
std::string underline(const std::string& line);

std::string red(const std::string& s);
std::string blue(const std::string& s);
#endif
