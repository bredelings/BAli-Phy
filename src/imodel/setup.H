#ifndef IMODEL_SETUP_H
#define IMODEL_SETUP_H

#include <string>
#include "computation/formula_expression.H"
#include "tree/sequencetree.H"

expression_ref get_imodel(std::string, const SequenceTree& T);

#endif
