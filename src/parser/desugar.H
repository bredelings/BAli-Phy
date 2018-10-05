#ifndef DESUGAR_H
#define DESUGAR_H

#include <set>
#include <string>
#include "computation/expression/expression_ref.H"

class Module;
class Model;

expression_ref desugar(const Module& p, const expression_ref&);
expression_ref desugar(const Module& p, const expression_ref&,const std::set<std::string>&);
Module read_model(const std::string& filename);
void read_add_model(Model& M, const std::string& filename);
void add_model(Model& M, const std::string& filename);

#endif
