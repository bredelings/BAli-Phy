#ifndef RUNTIME_VIEWS_H
#define RUNTIME_VIEWS_H

#include <optional>
#include <string>
#include "computation/expression/apply.H"
#include "computation/expression/case.H"
#include "computation/expression/constructor.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/trim.H"
#include "computation/operation.H"

namespace RuntimeView
{
    struct Lambda
    {
        expression_ref var;
        expression_ref body;
    };

    struct IndexedLambda
    {
        expression_ref body;
    };

    struct Let
    {
        const let_exp* value = nullptr;
    };

    struct Case
    {
        expression_ref object;
        Expression::CaseAlts alts;
    };

    struct Apply
    {
        const expression_ref* value = nullptr;
    };

    struct ConstructorApp
    {
        const expression_ref* value = nullptr;
    };

    struct OperationApp
    {
        const expression_ref* value = nullptr;
        const Operation* op = nullptr;
    };

    struct Trim
    {
        const expression_ref* value = nullptr;
    };

    inline std::optional<Lambda> lambda(const expression_ref& E)
    {
        if (E.head().is_a<::lambda>())
        {
            assert(E.size() == 2);
            return Lambda{E.sub()[0], E.sub()[1]};
        }

        return {};
    }

    inline std::optional<IndexedLambda> indexed_lambda(const expression_ref& E)
    {
        if (E.head().is_a<lambda2>())
        {
            assert(E.size() == 1);
            return IndexedLambda{E.sub()[0]};
        }

        return {};
    }

    inline std::optional<Let> let(const expression_ref& E)
    {
        if (is_let_expression(E))
            return Let{&E.as_<let_exp>()};

        return {};
    }

    inline std::optional<Case> case_(const expression_ref& E)
    {
        if (auto C = parse_case_expression(E))
        {
            auto& [object, alts] = *C;
            return Case{object, alts};
        }

        return {};
    }

    inline std::optional<Apply> apply(const expression_ref& E)
    {
        if (is_apply_exp(E))
            return Apply{&E};

        return {};
    }

    inline std::optional<ConstructorApp> constructor_app(const expression_ref& E)
    {
        if (is_constructor_exp(E))
            return ConstructorApp{&E};

        return {};
    }

    inline std::optional<OperationApp> operation_app(const expression_ref& E)
    {
        if (auto O = E.head().to<Operation>())
            return OperationApp{&E, O};

        return {};
    }

    inline std::optional<Trim> trim(const expression_ref& E)
    {
        if (E.head().is_a<::Trim>())
            return Trim{&E};

        return {};
    }
}

#endif
