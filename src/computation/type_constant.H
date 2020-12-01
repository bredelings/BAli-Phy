#ifndef TYPE_CONSTANT_H
#define TYPE_CONSTANT_H

// Note that expression_ref does: `bool is_object_type() const {return type_ > index_var_type;}`

enum type_constant {null_type=0,
                    int_type=1,
                    double_type=2,
                    log_double_type=3,
                    char_type=4,
                    index_var_type=5,
                    expression_type=6,
                    operation_type=7,
                    eptree_type = 8,
                    modifiable_type=9,
                    random_variable_type=10,
                    var_type=11,
                    trim_type=12,
                    let_type=13,
                    case_type=14,
                    apply_type=15,
                    let2_type=16,
                    constructor_type=17,
                    lambda_type=18,
                    lambda2_type=19,
                    seq_type=20,
                    join_type=21,
                    reg_var_type=22,
                    get_type=23,
                    unknown_type
};

#endif
