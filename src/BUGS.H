#ifndef BUGS_H
#define BUGS_H
#include <string>
#include <vector>
#include <map>
#include "parameters.H"

void add_BUGS(const Parameters& P, const std::string& filename);
expression_ref postprocess(const Program& p, const expression_ref&);
#endif
