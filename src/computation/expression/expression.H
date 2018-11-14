#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <vector>
#include <valarray>
#include <set>
#include <string>
#include <initializer_list>
#include "object.H"
#include "operator.H"
#include "computation/type_constant.H"
#include "expression_ref.H"
#include "constructor.H"
#include "bool.H"

/// a de Bruijn indexed variable 
struct index_var
{
    int index;
    index_var(int i):index(i) {}
};

struct parameter: public Object
{
    std::string parameter_name;

    parameter* clone() const {return new parameter(*this);}
    std::string print() const {return parameter_name;}
    bool operator==(const Object& o) const;
  
    type_constant type() const {return parameter_type;}

    parameter(const std::string& s);
};

struct reg_var: public Object
{
    int target;

    reg_var* clone() const {return new reg_var(*this);}

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

bool is_parameter(const expression_ref&);

bool is_modifiable(const expression_ref&);

bool is_reglike(const expression_ref& R);

std::set<std::string> find_named_parameters(const expression_ref&);

std::set<std::string> find_named_parameters(const std::vector<expression_ref>&);

expression_ref launchbury_normalize(const expression_ref& E);
expression_ref launchbury_unnormalize(const expression_ref& E);

expression_ref add_prefix(const std::string&, const expression_ref& E);

bool is_WHNF(const expression_ref& E);

expression_ref parse_object(const std::string& s);

std::vector<double> vec_to_double(const std::vector<expression_ref>& v);
std::vector<int> vec_to_int(const std::vector<expression_ref>& v);
std::vector<char> vec_to_char(const std::vector<expression_ref>& v);

#endif
