#ifndef STARTUP_CMD_LINE_H
#define STARTUP_CMD_LINE_H

#include <vector>
#include <string>
#include <boost/program_options.hpp>

extern const std::string trailing_args_separator;

std::vector<std::string> trailing_args(int argc, char* argv[], const std::string& separator);
boost::program_options::variables_map parse_cmd_line(int argc,char* argv[]);
std::string get_command_line(int argc, char* argv[]);

#endif
