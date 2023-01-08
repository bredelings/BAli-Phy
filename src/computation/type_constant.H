#ifndef TYPE_CONSTANT_H
#define TYPE_CONSTANT_H

// Note that expression_ref does: `bool is_object_type() const {return type_ > index_var_type;}`

enum type_constant {null_type=0,
                    int_type=1,
                    double_type=2,
                    log_double_type=3,
                    char_type=4,
                    index_var_type=5,

                    unknown_type = 10,

                    var_type=20,
                    reg_var_type=21,

                    lambda_type=30,
                    lambda2_type=31,
                    constructor_type=32,

                    expression_type=40,
                    eptree_type = 41,
                    modifiable_type=42,
                    trim_type=43,
                    let_type=44,

                    operation_type=50,
                    let2_type=51,
                    seq_type=52,
                    join_type=53,
                    case_type=54,
                    apply_type=55,

                    // anything with type >= 100 is gc_able
                    gc_type = 100
};

inline bool is_object_type(type_constant c) { return (c > index_var_type);}

inline bool is_literal_type(type_constant c) { return (c >=1 and c <=4) or (c==unknown_type);}

inline bool is_operation_type(type_constant c) { return (c >= operation_type);}

inline bool is_literal_or_var_type(type_constant c) {return (c >=1 and c <= reg_var_type);}

inline bool is_whnf_type(type_constant c) {return (c >=1 and c <= constructor_type);}

#endif
