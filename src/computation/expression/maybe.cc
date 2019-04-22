#include "computation/expression/maybe.H"
#include "computation/expression/var.H"

expression_ref Just = var("Data.Maybe.Just");
expression_ref Nothing = var("Data.Maybe.Nothing");
expression_ref fromJust = var("Data.Maybe.fromJust");
