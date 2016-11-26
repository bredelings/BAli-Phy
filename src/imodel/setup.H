#ifndef IMODEL_SETUP_H
#define IMODEL_SETUP_H

#include <string>
#include "tree/sequencetree.H"
#include "computation/expression.H"

expression_ref get_imodel(std::string, const SequenceTree& T);

#endif
