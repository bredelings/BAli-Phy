#ifndef TYPE_CONSTANT_H
#define TYPE_CONSTANT_H

// Note that expression_ref does: `bool is_object_type() const {return type_ > index_var_type;}`

enum type_constant {null_type,
                    int_type,
                    double_type,
                    log_double_type,
                    char_type,
                    index_var_type,

                    unknown_type = 10,

                    var_type=20,
                    reg_var_type,

                    lambda_type=30,
                    lambda2_type,
                    constructor_type,

                    // anything with type >= 100 is gc_able
                    gc_type = 100,

                    expression_type=200,
                    eptree_type,
                    modifiable_type,
                    interchangeable_type,
                    trim_type,
                    let_type,

                    operation_type,
                    let2_type,
                    seq_type,
                    join_type,
                    case_type,
                    apply_type,
};

inline bool is_object_type(type_constant c) { return (c > index_var_type);}

inline bool is_operation_type(type_constant c) { return (c >= operation_type);}

inline bool is_literal_or_var_type(type_constant c) {return (c >=1 and c <= reg_var_type);}

inline bool is_whnf_type(type_constant c) {return (c >=1 and c < expression_type);}

inline bool is_gcable_type(type_constant c) {return c >= gc_type and c < expression_type;}

inline bool is_literal_type(type_constant c) { return (c >=1 and c <=4) or (c==unknown_type) or is_gcable_type(c);}

#endif
