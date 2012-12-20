#ifndef PRELUDE_H
#define PRELUDE_H

#include <string>
#include "computation/program.H"

const Program& get_Prelude();

Program load_module(const std::vector<std::string>&, const std::string&);

#endif
