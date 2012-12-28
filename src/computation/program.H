#ifndef PROGRAM_H
#define PROGRAM_H

#include <vector>
#include <string>

class Module;

typedef std::vector<Module> Program;

bool contains_module(const std::vector<Module>& P, const std::string& module_name);

#endif
