#ifndef TEXT_H
#define TEXT_H

#include <string>
#include <list>
#include <vector>

int terminal_width();

namespace ANSI
{
    extern const std::string plain;
    extern const std::string bold;
    extern const std::string under;
    extern const std::string inverse;
    extern const std::string bold_off;
    extern const std::string under_off;
    extern const std::string inverse_off;
    extern const std::string black;
    extern const std::string bold_black;
    extern const std::string red;
    extern const std::string bold_red;
    extern const std::string green;
    extern const std::string bold_green;
    extern const std::string yellow;
    extern const std::string blue;
    extern const std::string bold_blue;
    extern const std::string magenta;
    extern const std::string cyan;

    extern const std::string bg_grey;
    extern const std::string bg_grey2;
}

std::string show_options(const std::vector<std::string>&);

std::string pad(const std::string& s, int n);
std::vector<std::string> get_lines(const std::string& line);

std::string indent_and_wrap(int indent_first_line, int indent_other_lines, int width, const std::string& text);
std::string indent_and_wrap(int indent, int width, const std::string& text);

std::string indent_and_wrap_par(int indent, int width, const std::string& text);
std::string indent_and_wrap_pars(int indent, int width, const std::string& text);

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
