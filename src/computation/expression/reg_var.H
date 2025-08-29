#ifndef REG_VAR_H
#define REG_VAR_H

#include <string>
#include "computation/object.H"
#include "computation/type_constant.H"
#include "computation/expression/expression_ref.H"

struct reg_var: public Object
{
    int target;

    reg_var* clone() const {return new reg_var(*this);}

    type_constant type() const {return type_constant::reg_var_type;}
    std::string print() const 
	{
	    return "<" + convertToString(target) + ">";
	}

    bool operator==(const Object& o) const
	{
	    const reg_var* E = dynamic_cast<const reg_var*>(&o);
	    if (not E) 
		return false;

	    return target == E->target;
	}

    reg_var():
	target(-1)
	{ }

    reg_var(int t):
	target(t)
	{ }
};

bool is_reg_var(const expression_ref& R);

#endif
