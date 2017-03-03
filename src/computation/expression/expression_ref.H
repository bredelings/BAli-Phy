#ifndef EXPRESSION_REF_H
#define EXPRESSION_REF_H

#include <vector>
#include <valarray>
#include <set>
#include <string>
#include <initializer_list>
#include "object.H"
#include "computation/operator.H"
#include "computation/type_constant.H"

/// An S-expression: An Object, and possibly an array of Objects.
struct expression;
struct index_var;

class expression_ref
{
    union
    {
	char c;
	int i;
	double d;
	log_double_t ld;
	object_ptr<const Object> O;
    };
    type_constant type_ = null_type;

public:

    bool is_object_type() const {return type_ > index_var_type;}

    tribool operator==(const expression_ref& E2) const
	{
	    if (type_ != E2.type())
		return false;
    
	    switch(type_)
	    {
	    case int_type:
		return (i == E2.as_int());
		break;
	    case double_type:
		return (d == E2.as_double());
		break;
	    case log_double_type:
		return (ld == E2.as_log_double());
		break;
	    case char_type:
		return (c == E2.as_char());
		break;
	    case index_var_type:
		return (i == E2.as_index_var());
		break;
	    default:
		return ptr()->compare(*E2.ptr());
	    }
	}

    tribool operator==(const Object& O) const
	{
	    if (not is_object_type())
		return false;
	    else
		return ptr()->compare(O);
	}

    void clear() {destroy_ptr();type_=null_type;}

    bool is_expression() const;

    bool is_atomic() const
	{
	    return not is_expression();
	}

    type_constant type() const
	{
	    return type_;
	}
  
    const expression& as_expression() const
	{
	    assert(not is_atomic());
	    return *convert<expression>(ptr().get());
	}

    inline const expression_ref& head() const;

    inline const std::vector<expression_ref>& sub() const;

    inline int size() const 
	{
	    if (is_atomic())
		return 0;
	    else
		return sub().size();
	}

    // do i still need this?
    expression_ref operator()(const std::vector<expression_ref>& args) const;

    template <typename T>
    bool is_a() const
	{
	    if (not is_object_type())
		return false;
	    else
		return (bool)boost::dynamic_pointer_cast<const T>(ptr());
	}

    template <typename T>
    object_ptr<const T> assert_is_a() const
	{
	    return convert<const T>(ptr());
	}

    template <typename T>
    const T& as_() const
	{
	    return *convert<const T>(ptr());
	}

    bool is_int() const
	{
	    return type_ == int_type;
	}

    int as_int() const
	{
	    assert(is_int());
	    return i;
	}

    bool is_double() const
	{
	    return type_ == double_type;
	}

    double as_double() const
	{
	    assert(is_double());
	    return d;
	}

    bool is_char() const
	{
	    return type_ == char_type;
	}

    char as_char() const
	{
	    assert(is_char());
	    return c;
	}

    bool is_log_double() const
	{
	    return type_ == log_double_type;
	}

    log_double_t as_log_double() const
	{
	    assert(is_log_double());
	    return ld;
	}

    bool is_index_var() const
	{
	    return type_ == index_var_type;
	}
  
    int as_index_var() const
	{
	    assert(is_index_var());
	    return i;
	}

    inline const object_ptr<const Object>& ptr() const
	{
	    assert(is_object_type());
	    assert((bool)O);
	    return O;
	}

    inline explicit operator bool() const
	{
	    return (type_ != null_type);
	}

    std::string print() const
	{
	    switch(type_)
	    {
	    case null_type:
		return "[NULL]";
	    case int_type:
		return convertToString(i);
		break;
	    case double_type:
		return convertToString(d);
		break;
	    case log_double_type:
		return convertToString(ld);
		break;
	    case char_type:
		return std::string("'")+c+"'";
		break;
	    case index_var_type:
		return std::string("%")+convertToString(i);
		break;
	    default:
		return ptr()->print();
	    }
	}

    typedef object_ptr<const Object> otype;

    inline void destroy_ptr()
	{
	    if (is_object_type())
		(&O)->~otype();
	}

    // Default
    expression_ref() {}

    expression_ref(expression* e);

    expression_ref(const expression* e);

    expression_ref(const object_ptr<const expression>& e):expression_ref(e.get()) { }

    expression_ref(const object_ptr<expression>& e):expression_ref(e.get()) { }

    expression_ref(const expression_ref& e, const std::initializer_list<expression_ref> v);

    expression_ref(const expression_ref& e, const std::vector<expression_ref>& v);

    // Object arguments
    expression_ref(Object* v):O(v),type_(v->type()) { }

    expression_ref(const Object* v):O(v),type_(v->type()) { }

    expression_ref(const object_ptr<Object>& v):expression_ref(v.get()) { }

    template <typename T>
    expression_ref(const object_ptr<T>& v):expression_ref(v.get()) { }

    expression_ref(const Object& o):expression_ref(o.clone()) { }

    // Builtin-type arguments
    expression_ref(const int& i2):i(i2),type_(int_type) {}
    expression_ref(const double& d2):d(d2),type_(double_type) {}
    expression_ref(const log_double_t& ld2):ld(ld2),type_(log_double_type) {}
    expression_ref(const bool& b);
    expression_ref(const index_var& iv);
    expression_ref(const std::string& s);
    expression_ref(const char* s);
    expression_ref(const char& c2):c(c2),type_(char_type) {}
    expression_ref(const String& s):expression_ref(s.clone()) { }

    expression_ref& operator=(const expression_ref& E)
	{
	    destroy_ptr();
	    type_ = E.type_;
	    if (is_object_type())
		new (&O) otype(E.ptr());
	    else
		d = E.d;
	    return *this;
	}

    expression_ref(const expression_ref& E):type_(E.type_)
	{
	    if (is_object_type())
		new (&O) otype(E.ptr());
	    else
		d = E.d;
	}

    inline ~expression_ref() {destroy_ptr();}
};


/// This represents function application: a,b,c = a(b)(c) and not a(b(c)). The precedence is 18 (low).
expression_ref operator,(const expression_ref&, const expression_ref&);

/// This represents the Cons constructor: a&b = Cons a b = a:b.  The precedence is 3 (high).
expression_ref operator&(const expression_ref&, const expression_ref&);

/// This does lambda-quantify
expression_ref operator^(const expression_ref& x, const expression_ref& T);

expression_ref operator+(const expression_ref& E1, const expression_ref& E2);

expression_ref operator*(const expression_ref& E1, const expression_ref& E2);

/*
 *  By separating out the head from the children, each expression now represents
 *  a node in a directed acyclic graph, where each node has an Object on in.
 */

/// An array of Objects.
struct expression: public Object
{
    // The head of the expression.
    expression_ref head;

    // An expression may or may not have sub-expressions
    std::vector< expression_ref > sub;

    /// The number of terms after the head
    int size() const {return sub.size();}
  
    // Methods from Object
    expression* clone() const {return new expression(*this);}
    tribool compare(const Object& o) const;
    std::string print() const;
  
    tribool operator==(const expression& E) const;

    type_constant type() const {return expression_type;}

    expression() {}

    expression(const expression_ref&);

    expression(const expression_ref&, const std::initializer_list< expression_ref > S);

    expression(const expression_ref&, const std::vector< expression_ref >& S);

    virtual ~expression() {}
};

inline const expression_ref& expression_ref::head() const 
{
    if (is_expression())
	return as_expression().head;
    else
	return *this;
}

inline bool expression_ref::is_expression() const
{
    return (type_ == expression_type);
}

inline const std::vector<expression_ref>& expression_ref::sub() const {return as_expression().sub;}
//        object_ptr<const Object>& sub()       {return E->sub;}

inline expression_ref::expression_ref(expression* e):O(e),type_(expression_type) { }

inline expression_ref::expression_ref(const expression* e):O(e),type_(expression_type) { }

inline expression_ref::expression_ref(const expression_ref& e, const std::initializer_list<expression_ref> v):expression_ref(new expression(e,v)) { }

inline expression_ref::expression_ref(const expression_ref& e, const std::vector<expression_ref>& v):expression_ref(new expression(e,v)) { }

inline std::ostream& operator<<(std::ostream& o,const expression_ref& E)
{
    if (E)
	return o<<E.print();
    else
	return o<<"[NULL]";
}

#endif
