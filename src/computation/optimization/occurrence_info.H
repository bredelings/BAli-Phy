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

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(work_dup, code_dup, context, is_loop_breaker);
    }
};

namespace Occ
{
    using Var = Core::Var<occurrence_info>;
    using Lambda = Core::Lambda<occurrence_info>;
    using Apply = Core::Apply<occurrence_info>;
    using Let = Core::Let<occurrence_info>;
    using Decl = Core::Decl<occurrence_info>;
    using Decls = Core::Decls<occurrence_info>;
    using NonRec = Core::NonRec<occurrence_info>;
    using Rec = Core::Rec<occurrence_info>;
    using Bind = Core::Bind<occurrence_info>;
    using Binds = Core::Binds<occurrence_info>;
    using Case = Core::Case<occurrence_info>;
    using Pattern = Core::Pattern<occurrence_info>;
    using Alt = Core::Alt<occurrence_info>;
    using ConApp = Core::ConApp<occurrence_info>;
    using BuiltinOp = Core::BuiltinOp<occurrence_info>;
    using Constant = Core::Constant;
    using Exp = Core::Exp<occurrence_info>;
    using subst_t = Core::subst_t<occurrence_info>;
};

#endif
