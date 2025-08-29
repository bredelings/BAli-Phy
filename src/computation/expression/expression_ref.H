#ifndef EXPRESSION_REF_H
#define EXPRESSION_REF_H

#include <vector>
#include <memory>
#include <optional>
#include <string>
#include <initializer_list>
#include "computation/object.H"
#include "util/string/convert.H"
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
    type_constant type_ = type_constant::null_type;

public:

    bool is_object_type() const {return ::is_object_type(type_);}

    bool operator==(const expression_ref& E2) const
	{
	    if (type_ != E2.type())
		return false;
    
	    switch(type_)
	    {
	    case type_constant::null_type:
		return true;
		break;
	    case type_constant::int_type:
		return (i == E2.as_int());
		break;
	    case type_constant::double_type:
		return (d == E2.as_double());
		break;
	    case type_constant::log_double_type:
		return (ld == E2.as_log_double());
		break;
	    case type_constant::char_type:
		return (c == E2.as_char());
		break;
	    case type_constant::index_var_type:
		return (i == E2.as_index_var());
		break;
	    default:
                if (ptr() == E2.ptr()) return true;

		return (*ptr() == *E2.ptr());
	    }
	}

    bool operator==(const Object& O) const
	{
	    if (not is_object_type())
		return false;
	    else
		return (*ptr() == O);
	}

    void clear() {destroy_ptr();type_=type_constant::null_type;}

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

    inline std::vector<expression_ref> copy_sub() const;

    inline int size() const 
	{
	    if (is_atomic())
		return 0;
	    else
		return sub().size();
	}

    template <typename T>
    const T* to() const
	{
	    if (not is_object_type())
		return nullptr;
	    else
		return dynamic_cast<const T*>(ptr().get());
	}

    template <typename T1, typename T2>
    const T2* poly_to() const
	{
	    if (not is_object_type())
		return nullptr;
            const T1* converted1 = dynamic_cast<const PtrBox<T1>*>(ptr().get())->get();
            if (not converted1)
                return false;
            return dynamic_cast<const T2*>(converted1);
	}

    template <typename T>
    bool is_a() const
    {
        return (bool)to<T>();
    }

    template <typename T1, typename T2>
    bool poly_is_a() const
    {
        return (bool)poly_to<T1,T2>();
    }

    template <typename T1, typename T2>
    std::shared_ptr<const T2> poly_cast() const
	{
	    if (not is_object_type())
		return nullptr;

            std::shared_ptr<const T1> converted1 = *dynamic_cast<const PtrBox<T1>*>(ptr().get());

            if (not converted1)
                return nullptr;

            return std::dynamic_pointer_cast<const T2>(std::move(converted1));
	}

    template <typename T>
    object_ptr<const T> as_ptr_to() const
	{
	    return convert<const T>(ptr());
	}

    template <typename T>
    const T& as_() const
	{
	    return *convert<const T>(ptr().get());
	}

    template <typename T1, typename T2>
    std::shared_ptr<const T2> poly_as_() const
	{
	    return poly_convert<T1,T2>(ptr().get());
	}

    template <typename T>
    const T& as_checked() const
	{
	    return *convert_and_check<const T>(ptr().get());
	}

    bool is_int() const
	{
	    return type_ == type_constant::int_type;
	}

    int as_int() const
	{
	    if (not is_int())
		throw myexception()<<"Treating '"<<*this<<"' as int!";
	    return i;
	}

    bool is_double() const
	{
	    return type_ == type_constant::double_type;
	}

    double as_double() const
	{
	    if (not is_double())
		throw myexception()<<"Treating '"<<*this<<"' as double!";
	    return d;
	}

    bool is_char() const
	{
	    return type_ == type_constant::char_type;
	}

    char as_char() const
	{
	    if (not is_char())
		throw myexception()<<"Treating '"<<*this<<"' as char!";
	    return c;
	}

    bool is_log_double() const
	{
	    return type_ == type_constant::log_double_type;
	}

    log_double_t as_log_double() const
	{
	    if (not is_log_double())
		throw myexception()<<"Treating '"<<*this<<"' as log_double!";
	    return ld;
	}

    bool is_index_var() const
	{
	    return type_ == type_constant::index_var_type;
	}
  
    int as_index_var() const
	{
	    if (not is_index_var())
		throw myexception()<<"Treating '"<<*this<<"' as index_var!";
	    return i;
	}

    inline const object_ptr<const Object>& ptr() const
	{
	    if (not is_object_type())
		throw myexception()<<"Treating '"<<*this<<"' as object type!";
	    assert((bool)O);
	    return O;
	}

    inline explicit operator bool() const
	{
	    return (type_ != type_constant::null_type);
	}

    std::string print() const;

    typedef object_ptr<const Object> otype;

    inline void destroy_ptr()
	{
	    if (is_object_type())
		(&O)->~otype();
	}

    // Default
    expression_ref() {d = 0;}

    expression_ref(expression* e);

    expression_ref(const expression* e);

    expression_ref(std::unique_ptr<expression> e):expression_ref(e.release()) {};

    expression_ref(const object_ptr<const expression>& e):expression_ref(e.get()) { }

    expression_ref(const object_ptr<expression>& e):expression_ref(e.get()) { }

    explicit expression_ref(const expression_ref& e, const std::vector<expression_ref>& v);

    // Object arguments
    expression_ref(Object* v):O(v),type_(v->type()) { }

    expression_ref(const Object* v):O(v),type_(v->type()) { }

    expression_ref(const object_ptr<Object>& v):expression_ref(v.get()) { }

    template <typename T>
    expression_ref(const object_ptr<T>& v):expression_ref(v.get()) { }

    expression_ref(const Object& o):expression_ref(o.clone()) { }

    // Builtin-type arguments
    expression_ref(const int& i2):i(i2),type_(type_constant::int_type) {}
    expression_ref(const double& d2):d(d2),type_(type_constant::double_type) {}
    expression_ref(const log_double_t& ld2):ld(ld2),type_(type_constant::log_double_type) {}
    expression_ref(const bool& b);
    expression_ref(const index_var& iv);
    expression_ref(const std::string& s) = delete;
    expression_ref(const char* s) = delete;
    expression_ref(const char& c2):c(c2),type_(type_constant::char_type) {}
    expression_ref(const String& s):expression_ref(s.clone()) { }
    expression_ref(const std::initializer_list<expression_ref>&);

    void swap(expression_ref& E) noexcept
        {
            static_assert(sizeof(d) >= sizeof(O));
            std::swap(d, E.d);
            std::swap(type_, E.type_);
        }

    expression_ref& operator=(const expression_ref& E)
	{
            // If *this points to a structure containing E,
            // then E can get destroyed during the operation.
            // That means we can't access E.type_ after the assignment of E.O

            // By constructing a temporary here we
            // * increment the reference of E.O (if its a pointer)
            // * copy E.type_, so that we can use it safely.

            expression_ref(E).swap(*this);
	    return *this;
	}

    expression_ref& operator=(expression_ref&& E) noexcept
	{
            swap(E);
	    return *this;
	}

    expression_ref(const expression_ref& E):type_(E.type_)
	{
	    if (is_object_type())
		new (&O) otype(E.ptr());
	    else
		d = E.d;
	}

    expression_ref(expression_ref&& E) noexcept
	{
	    d = 0;
	    swap(E);
	}

    inline ~expression_ref() {destroy_ptr();}
};


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
    std::string print() const;
  
    bool operator==(const expression& E) const;
    bool operator==(const Object& o) const;

    type_constant type() const {return type_constant::expression_type;}

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
    return (type_ == type_constant::expression_type);
}

inline std::vector<expression_ref> expression_ref::copy_sub() const
{
    if (is_expression())
	return as_expression().sub;
    else
	return {};
}

inline const std::vector<expression_ref>& expression_ref::sub() const {return as_expression().sub;}

inline expression_ref::expression_ref(expression* e):O(e),type_(type_constant::expression_type) { }

inline expression_ref::expression_ref(const expression* e):O(e),type_(type_constant::expression_type) { }

inline expression_ref::expression_ref(const expression_ref& e, const std::vector<expression_ref>& v):expression_ref(new expression(e,v)) { }

inline std::ostream& operator<<(std::ostream& o,const expression_ref& E)
{
    return o<<E.print();
}

struct EVector: public Vector<expression_ref>
{
    // Workaround for GCC6
    EVector() = default;

    using Vector<expression_ref>::Vector;
    
    EVector* clone() const {return new EVector(*this);};

    explicit EVector(const std::vector<double>& v);
    explicit EVector(const std::vector<int>& v);
    explicit EVector(const std::vector<char>& v);

    explicit operator std::vector<double>() const;
    explicit operator std::vector<int>() const;
    explicit operator std::vector<char>() const;
};

struct EPair: public Pair<expression_ref,expression_ref>
{
    EPair() = default;

    using Pair<expression_ref,expression_ref>::Pair;

    EPair* clone() const {return new EPair(*this);};

    explicit EPair(const std::pair<int,int>& p);

    explicit operator std::pair<int,int>() const;
};


typedef std::pair<expression_ref,expression_ref> Epair;

typedef Box<std::optional<expression_ref>> EMaybe;

struct EPtree: public Vector<std::pair<std::string,expression_ref>>
{
    expression_ref head;

    EPtree* clone() const {return new EPtree(*this);}
    std::string print() const;

    bool operator==(const EPtree& E) const;
    bool operator==(const Object& o) const;

    type_constant type() const {return type_constant::eptree_type;}

    int count(const std::string& key) const;

    void erase(const std::string& key);

    std::optional<int> get_child_index(const std::string& key) const;

    expression_ref* get_child_optional(const std::string& key);

    const expression_ref* get_child_optional(const std::string& key) const;

    expression_ref& get_child(const std::string& key);

    const expression_ref& get_child(const std::string& key) const;

    EPtree() {}

    EPtree(const expression_ref&);

    EPtree(const expression_ref&, const std::initializer_list< std::pair<std::string, expression_ref> > S);

    EPtree(const expression_ref&, const std::vector< std::pair<std::string, expression_ref> > S);

    virtual ~EPtree() {}
};

template<>
inline std::string Box<std::vector<expression_ref>>::print() const
{
    if (empty()) return "{}";

    std::string s = "{";
    for(int i=0;i<(int)size()-1;i++) {
	s += (*this)[i].print();
	s += ",";
    }
    s += this->back().print() + "}";
    return s;
}

template<>
inline std::string Box<std::pair<expression_ref,expression_ref>>::print() const
{
    return std::string("(") + first.print() + "," + second.print() + ")";
}

std::unique_ptr<expression> operator+(const expression_ref& E1, const expression_ref& E2);
std::unique_ptr<expression> operator+(const expression& E1, const expression_ref& E2);
inline std::unique_ptr<expression> operator+(std::unique_ptr<expression> E1, const expression_ref& E2)
{
    E1->sub.push_back(E2);
    return E1;
}

#endif
