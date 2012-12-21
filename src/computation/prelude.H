#ifndef PRELUDE_H
#define PRELUDE_H

#include <string>
#include <vector>
#include "computation/program.H"

Program load_module(const std::vector<std::string>&, const std::string&);
std::vector<Program> load_modules(const std::vector<std::string>&, const std::vector<std::string>&);

#endif
