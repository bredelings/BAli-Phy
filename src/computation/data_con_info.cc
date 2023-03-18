#include "data_con_info.H"

using std::vector;


vector<Type> DataConInfo::all_constraints() const
{
    auto cs = top_constraints;
    for(auto& constraint: gadt_eq_constraints)
        cs.push_back(constraint);
    for(auto& constraint: written_constraints)
        cs.push_back(constraint);
    return cs;
}

vector<Type> DataConInfo::dictionary_preds() const
{
    return ::dictionary_preds(all_constraints());
}

vector<Type> DataConInfo::equality_preds() const
{
    return ::equality_preds(all_constraints());
}

int DataConInfo::dict_arity() const
{
    return dictionary_preds().size();
}

int DataConInfo::arity() const
{
    return field_types.size();
}

Type DataConInfo::result_type() const
{
    return type_apply( data_type, uni_tvs );
}

Type DataConInfo::constructor_type() const
{
    auto type = function_type(field_types, result_type());

    // add_constraints will merge the constraints, here.
    type = add_constraints(written_constraints, type);
    type = add_constraints(gadt_eq_constraints, type);
    type = add_forall_vars(exi_tvs, type);

    // FIXME: Only add top constraints for type variables in constructor fields
    type = add_constraints(top_constraints, type);
    type = add_forall_vars(uni_tvs, type);

    return type;
}

