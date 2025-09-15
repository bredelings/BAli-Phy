#ifndef OCCURRENCE_INFO_H
#define OCCURRENCE_INFO_H

#include "computation/core/ast.H"
#include "computation/core/subst.H"

enum class amount_t {Unknown, None, Once, Many};

enum class var_context {unknown, argument};

struct occurrence_info
{
    amount_t work_dup = amount_t::Unknown;

    amount_t code_dup = amount_t::Unknown;

    var_context context = var_context::unknown;

    bool is_loop_breaker = false;

    bool pre_inline() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(work_dup, code_dup, context, is_loop_breaker);
    }
};

namespace Occ
{
    using Var = Core2::Var<occurrence_info>;
    using Lambda = Core2::Lambda<occurrence_info>;
    using Apply = Core2::Apply<occurrence_info>;
    using Let = Core2::Let<occurrence_info>;
    using Decl = Core2::Decl<occurrence_info>;
    using Decls = Core2::Decls<occurrence_info>;
    using Case = Core2::Case<occurrence_info>;
    using Pattern = Core2::Pattern<occurrence_info>;
    using Alt = Core2::Alt<occurrence_info>;
    using ConApp = Core2::ConApp<occurrence_info>;
    using BuiltinOp = Core2::BuiltinOp<occurrence_info>;
    using Constant = Core2::Constant;
    using Exp = Core2::Exp<occurrence_info>;
    using subst_t = Core2::subst_t<occurrence_info>;
};

#endif
