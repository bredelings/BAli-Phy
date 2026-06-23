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
#include <concepts>

class expression_ref
{
    object_ptr<const Object> O;
    type_constant type_ = type_constant::null_type;

public:

    bool is_object_type() const {return ::is_object_type(type_);}

    bool operator==(const expression_ref& E2) const
	{
	    if (type_ != E2.type())
		return false;

	    if (type_ == type_constant::null_type)
		return true;

	    if (ptr() == E2.ptr()) return true;

	    return (*ptr() == *E2.ptr());
	}

    bool operator==(const Object& O) const
	{
	    if (not is_object_type())
		return false;
	    else
		return (*ptr() == O);
	}

    void clear() {O = {}; type_=type_constant::null_type;}

    type_constant type() const
        {
            return type_;
        }

    template <DerivedFromObject T>
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

    template <DerivedFromObject T>
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

    template <DerivedFromObject T>
    object_ptr<const T> as_ptr_to() const
	{
	    return convert<const T>(ptr());
	}

    template <DerivedFromObject T>
    const T& as_() const
	{
	    return *convert<const T>(ptr().get());
	}

    template <typename T1, typename T2>
    std::shared_ptr<const T2> poly_as_() const
	{
	    return poly_convert<T1,T2>(ptr().get());
	}

    template <DerivedFromObject T>
    const T& as_checked() const
	{
	    return *convert_and_check<const T>(ptr().get());
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

    // Default
    expression_ref() = default;

    // Object arguments
    expression_ref(Object* v):O(v),type_(v ? v->type() : type_constant::null_type) { }

    expression_ref(const Object* v):O(v),type_(v ? v->type() : type_constant::null_type) { }

    expression_ref(const object_ptr<Object>& v):expression_ref(v.get()) { }

    template <DerivedFromObject T>
    expression_ref(const object_ptr<T>& v):expression_ref(v.get()) { }

    expression_ref(const Object& o):expression_ref(o.clone()) { }

    // Builtin-type arguments
    expression_ref(const int& i2) = delete;
    expression_ref(const double& d2) = delete;
    expression_ref(const log_double_t& ld2) = delete;
    expression_ref(const bool& b) = delete;
    expression_ref(const std::string& s) = delete;
    expression_ref(const char* s) = delete;
    expression_ref(const char& c2) = delete;
    expression_ref(const String& s):expression_ref(s.clone()) { }

    void swap(expression_ref& E) noexcept
        {
            std::swap(O, E.O);
            std::swap(type_, E.type_);
        }

    expression_ref& operator=(const expression_ref& E) = default;

    expression_ref& operator=(expression_ref&& E) noexcept = default;

    expression_ref(const expression_ref& E) = default;

    expression_ref(expression_ref&& E) noexcept = default;

    ~expression_ref() = default;
};

inline std::ostream& operator<<(std::ostream& o,const expression_ref& E)
{
    return o<<E.print();
}

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

#endif
