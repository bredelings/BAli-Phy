#ifndef INLINER_H
#define INLINER_H

#include <vector>
#include <string>
#include "expression/AST_node.H"
#include "computation/expression/expression_ref.H"

class inline_context
{
    expression_ref data;
public:
    inline_context prev_context() const {
	if (data)
	    assert(data.head().is_a<AST_node>());
	return inline_context(data.sub()[1]);
    }

    bool is_case_object() const {return is_AST(data,"case_object");}
    bool is_apply_object() const {return is_AST(data,"apply_object");}
    bool is_argument() const {return is_AST(data,"argument");}
    bool is_unknown() const {return is_AST(data,"unknown");}
    bool is_null() const {return not data;}
    expression_ref get_expression() const {return data.sub()[0];};

    inline_context() {};
    inline_context(const expression_ref& E):data(E) {}
    inline_context(const std::string& s, const expression_ref E, const inline_context& context)
	:data(AST_node(s) + E + context.data) {}
};


inline_context unknown_context();
inline_context argument_context(const inline_context& context);
inline_context apply_object_context_one_arg(const inline_context& context, const expression_ref& arg);
inline_context apply_object_context(const expression_ref E, inline_context context);
inline_context case_object_context(const expression_ref E, const inline_context& context);

bool do_inline(const simplifier_options& options, const expression_ref& rhs, const occurrence_info& occur, const inline_context& context);

int simple_size(const expression_ref& E);

#endif
