#ifndef BUILTINS_VECTOR_H
#define BUILTINS_VECTOR_H

#include <limits>
#include "computation/closure.H"
#include "util/myexception.H"

// Validate a boxed Data.Vector closure and expose its lazy element registers
// without making separately loaded builtin modules share a helper symbol.
inline const closure::Env_t& boxed_vector_element_regs(const closure& value)
{
    auto app = value.get_code().to<Runtime::ConstructorApp>();
    if (not app or app->head.name() != "Vector")
        throw myexception()<<"Expression is not a boxed Vector: "
                           <<value.get_code().print();
    auto size = app->args.size();
    if (size > static_cast<std::size_t>(std::numeric_limits<int>::max()) or
        app->head.n_args() != static_cast<int>(size) or
        value.Env.size() != size)
        throw myexception()<<"Malformed boxed Vector representation with "
                           <<app->head.n_args()<<" declared fields, "
                           <<size<<" constructor fields, and "
                           <<value.Env.size()<<" environment registers";
    return value.Env;
}

#endif
