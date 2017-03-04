#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <vector>
#include <valarray>
#include <set>
#include <string>
#include <initializer_list>
#include "object.H"
#include "computation/operator.H"
#include "computation/type_constant.H"
#include "expression_ref.H"
#include "constructor.H"

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
    tribool compare(const Object& o) const;
  
    type_constant type() const {return parameter_type;}

    parameter(const std::string& s);
};

// a dummy variable expression
struct dummy: public Object
{
    int index;

    std::string name;

    bool operator==(const dummy&) const;
    dummy* clone() const {return new dummy(*this);}
    std::string print() const;
    tribool compare(const Object& o) const;
  
    bool operator<(const dummy&) const;

    type_constant type() const {return dummy_type;}

    bool is_wildcard() const {return index < 0 and not name.size();}

    dummy(int i):index(i) {}
    dummy(const std::string& s):dummy(s,-1) {}
    dummy(const std::string& s,int i):index(i),name(s) {assert(name != "_");}
};

struct reg_var: public Object
{
    int target;

    reg_var* clone() const {return new reg_var(*this);}

    std::string print() const 
	{
	    return "<" + convertToString(target) + ">";
	}

    tribool compare(const Object& o) const
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

struct identifier: public Object
{
    std::string name;

    identifier* clone() const {return new identifier(*this);}

    std::string print() const
	{
	    return name;
	}

    tribool compare(const Object& o) const
	{
	    const identifier* V = dynamic_cast<const identifier*>(&o);
	    if (not V)
		return false;

	    return name == V->name;
	}

    identifier(const std::string& s);
};

bool is_dummy(const expression_ref&);

bool is_parameter(const expression_ref&);

bool is_modifiable(const expression_ref&);

bool is_identifier(const expression_ref& R);

bool is_reglike(const expression_ref& R);

bool is_wildcard(const dummy& d);

bool is_wildcard(const expression_ref&);

bool is_irrefutable_pattern(const expression_ref& E);

std::set<std::string> find_named_parameters(const expression_ref&);

std::set<std::string> find_named_parameters(const std::vector<expression_ref>&);

expression_ref lambda_expression(const Operator& O);
expression_ref lambda_quantify(int dummy_index, const expression_ref& E);
expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& E);

int max_index(const std::set<dummy>& s);

std::set<dummy> get_free_indices(const expression_ref& E);

std::set<dummy> get_bound_indices(const expression_ref& E);

struct lambda: public Object
{
    lambda* clone() const {return new lambda(*this);}
    tribool compare(const Object& o) const;
    type_constant type() const {return lambda_type;}
    std::string print() const;
};

struct lambda2: public Object
{
    lambda2* clone() const {return new lambda2(*this);}
    tribool compare(const Object& o) const;

    type_constant type() const {return lambda2_type;}
    std::string print() const;
};

template <typename First, typename Second>
auto apply_subst(const First& first, const Second& second)
{
    return first * second;
}

template <typename First, typename Second, typename Third, typename... Rest>
auto apply_subst(const First& first, const Second& second, const Third& third, const Rest&... rest)
{
    return apply_subst(first*second, third, rest...);
}

template <typename First, typename Second, typename... Rest>
auto Tuple(const First& first, const Second& second, const Rest&... rest)
{
    return apply_subst(Tuple(2+sizeof...(rest)) ,first,second,rest...);
}

// 2 Fields, a value, and a Cons or a ListEnd
extern expression_ref Cons;

// 0 fields
extern expression_ref ListEnd;

inline expression_ref List() {return ListEnd;}

template <typename First>
auto List(const First& first)
{
    return Cons*first*ListEnd;
}

template <typename First, typename T2>
auto List(const First& first, const T2& t2)
{
    return Cons*first*List(t2);
}

template <typename First, typename T2, typename T3>
auto List(const First& first, const T2& t2, const T3& t3)
{
    return Cons*first*List(t2,t3);
}

template <typename First, typename T2, typename T3, typename T4>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4)
{
    return Cons*first*List(t2,t3,t4);
}

template <typename First, typename T2, typename T3, typename T4, typename T5>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4, const T5& t5)
{
    return Cons*first*List(t2,t3,t4,t5);
}


template <typename First, typename T2, typename T3, typename T4, typename T5, typename T6>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4, const T5& t5,const T6& t6)
{
    return Cons*first*List(t2,t3,t4,t5,t6);
}


template <typename T,typename U>
std::vector<T> get_vector(const expression_ref& E) 
{
    if (not E)
	return std::vector<T>();

    if (not E.size())
    {
	std::vector<T> v2(1);
	v2[0] = E.as_<U>();
	return v2;
    }
    
    std::vector<T> v2(E.size());
    for(int i=0;i<v2.size();i++)
	v2[i] = E.sub()[i].as_<U>();
    return v2;
}

std::vector<expression_ref> get_ref_vector_from_list(const expression_ref& E);

template <typename T,typename U>
std::vector<T> get_vector_from_list(const expression_ref& E)
{
    std::vector<expression_ref> v1 = get_ref_vector_from_list(E);
    std::vector<T> v2;
    for(int i=0;i<v1.size();i++)
	v2.push_back(v1[i].as_<U>());
    return v2;
}

template <typename T>
expression_ref get_tuple(const std::vector<T>& v)
{
    std::vector<expression_ref> v2;
    for(int i=0;i<v.size();i++)
	v2.push_back(v[i]);

    return get_tuple(v2);
}

template<> expression_ref get_tuple<>(const std::vector<expression_ref>& v);

template <typename T>
expression_ref get_tuple(const std::valarray<T>& v)
{
    std::vector<expression_ref> v2;
    for(int i=0;i<v.size();i++)
	v2.push_back(v[i]);

    return get_tuple(v2);
}

template <typename T>
expression_ref get_list(const std::vector<T>& v)
{
    std::vector<expression_ref> v2;
    for(int i=0;i<v.size();i++)
	v2.push_back(v[i]);

    return get_list(v2);
}

template<> expression_ref get_list<>(const std::vector<expression_ref>& v);

expression_ref launchbury_normalize(const expression_ref& E);
expression_ref launchbury_unnormalize(const expression_ref& E);

expression_ref add_prefix(const std::string&, const expression_ref& E);

bool is_WHNF(const expression_ref& E);

int get_safe_binder_index(const expression_ref& E);

expression_ref char_list(const std::string& s);

extern const expression_ref v0;
extern const expression_ref v1;
extern const expression_ref v2;
extern const expression_ref v3;
extern const expression_ref v4;
extern const expression_ref v5;
extern const expression_ref v6;
extern const expression_ref v7;
extern const expression_ref v8;

expression_ref parse_object(const std::string& s);

std::vector<double> vec_to_double(const std::vector<expression_ref>& v);
std::vector<int> vec_to_int(const std::vector<expression_ref>& v);

#endif
