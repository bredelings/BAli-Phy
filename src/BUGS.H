#ifndef BUGS_H
#define BUGS_H
#include <string>
#include <vector>
#include <map>

class Parameters;
class Program;
class expression_ref;

void add_BUGS(const Parameters& P, const std::string& filename);
expression_ref postprocess(const Program& p, const expression_ref&);
#endif
