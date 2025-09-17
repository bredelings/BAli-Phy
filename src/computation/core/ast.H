#ifndef CORE_AST_H
#define CORE_AST_H

#include <iostream>
#include <optional>
#include <variant>
#include <vector>
#include <string>
#include "util/cow-ptr.H"
#include "util/string/join.H"
#include "util/math/log-double.H"
#include "computation/haskell/ids.H"
#include "computation/haskell/integer_container.H"

#include <cereal/types/memory.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/tuple.hpp>
#include <cereal/types/variant.hpp>
#include <cereal/types/vector.hpp>

#include "range/v3/all.hpp"

/* TODO:
 *
 * 3. Make Apply take exactly one argument.
 * 4. Don't allow empty Exp -- actually this prevents cereal from serializing it.
 *    Use optional<Exp> instead?
 */

namespace Core2
{
    template <typename NoteV> struct Var;
    template <typename NoteV, typename NoteE> struct Lambda;
    template <typename NoteV, typename NoteE> struct Apply;
    template <typename NoteV, typename NoteE> struct Let;
    template <typename NoteV, typename NoteE> struct Case;
    template <typename NoteV> struct ConApp;
    template <typename NoteV> struct BuiltinOp;
    struct Constant;

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Exp
    {
	std::variant
	<
	    std::monostate,
	    cow_ptr<Var<NoteV>>,
	    cow_ptr<Lambda<NoteV,NoteE>>,
	    cow_ptr<Apply<NoteV,NoteE>>,
	    cow_ptr<Let<NoteV,NoteE>>,
	    cow_ptr<Case<NoteV,NoteE>>,
	    cow_ptr<ConApp<NoteV>>,
	    cow_ptr<BuiltinOp<NoteV>>,
	    cow_ptr<Constant>
	>
	exp_ptr;

	NoteE note_;

        const NoteE& note() const {return note_;}
              NoteE& note()       {return note_;}

        std::string print() const;

	bool empty() const
	{
            assert(not exp_ptr.valueless_by_exception());
            return std::holds_alternative<std::monostate>(exp_ptr);
	}

	const Var<NoteV>* to_var() const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 1)
		return std::get<cow_ptr<Var<NoteV>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Var<NoteV>* to_var_modify()
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 1)
		return std::get<cow_ptr<Var<NoteV>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Lambda<NoteV,NoteE>* to_lambda() const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 2)
		return std::get<cow_ptr<Lambda<NoteV,NoteE>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Lambda<NoteV,NoteE>* to_lambda_modify()
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 2)
		return std::get<cow_ptr<Lambda<NoteV,NoteE>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Apply<NoteV,NoteE>* to_apply() const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 3)
		return std::get<cow_ptr<Apply<NoteV,NoteE>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Apply<NoteV,NoteE>* to_apply_modify()
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 3)
		return std::get<cow_ptr<Apply<NoteV,NoteE>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Let<NoteV,NoteE>* to_let() const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 4)
		return std::get<cow_ptr<Let<NoteV,NoteE>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Let<NoteV,NoteE>* to_let_modify()
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 4)
		return std::get<cow_ptr<Let<NoteV,NoteE>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Case<NoteV,NoteE>* to_case() const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 5)
		return std::get<cow_ptr<Case<NoteV,NoteE>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Case<NoteV,NoteE>* to_case_modify()
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 5)
		return std::get<cow_ptr<Case<NoteV,NoteE>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const ConApp<NoteV>* to_conApp() const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 6)
		return std::get<cow_ptr<ConApp<NoteV>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	ConApp<NoteV>* to_conApp_modify()
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 6)
		return std::get<cow_ptr<ConApp<NoteV>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const BuiltinOp<NoteV>* to_builtinOp() const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 7)
		return std::get<cow_ptr<BuiltinOp<NoteV>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	BuiltinOp<NoteV>* to_builtinOp_modify()
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 7)
		return std::get<cow_ptr<BuiltinOp<NoteV>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Constant* to_constant() const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 8)
		return std::get<cow_ptr<Constant>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Constant* to_constant_modify()
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() == 8)
		return std::get<cow_ptr<Constant>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	bool operator==(const Exp<NoteV,NoteE>& E2) const
	{
            assert(not exp_ptr.valueless_by_exception());
	    if (exp_ptr.index() != E2.exp_ptr.index()) return false;

	    if (empty())
		return true;
	    else if (auto v= to_var())
		return *v == *E2.to_var();
	    else if (auto lam = to_lambda())
		return *lam == *E2.to_lambda();
	    else if (auto app = to_apply())
		return *app == *E2.to_apply();
	    else if (auto let = to_let())
		return *let == *E2.to_let();
	    else if (auto c = to_case())
		return *c == *E2.to_case();
	    else if (auto con = to_conApp())
		return *con == *E2.to_conApp();
	    else if (auto b = to_builtinOp())
		return *b == *E2.to_builtinOp();
	    else if (auto constant = to_constant())
		return *constant == *E2.to_constant();
	    else
		std::abort();
	}

	std::strong_ordering operator<=>(const Exp<NoteV,NoteE>& E2) const
	{
            assert(not exp_ptr.valueless_by_exception());
	    auto cmp1 = exp_ptr.index() <=> E2.exp_ptr.index();

	    if (cmp1 != std::strong_ordering::equivalent) return cmp1;

	    if (empty())
		return cmp1;
	    else if (auto v= to_var())
		return *v <=> *E2.to_var();
	    else if (auto lam = to_lambda())
		return *lam <=> *E2.to_lambda();
	    else if (auto app = to_apply())
		return *app <=> *E2.to_apply();
	    else if (auto let = to_let())
		return *let <=> *E2.to_let();
	    else if (auto c = to_case())
		return *c <=> *E2.to_case();
	    else if (auto con = to_conApp())
		return *con <=> *E2.to_conApp();
	    else if (auto b = to_builtinOp())
		return *b <=> *E2.to_builtinOp();
	    else if (auto constant = to_constant())
		return *constant <=> *E2.to_constant();
	    else
		std::abort();
	}

        Exp<NoteV,NoteE>& operator=(const Exp<NoteV,NoteE>& E)
        {
            assert(not E.exp_ptr.valueless_by_exception());
            // If *this points to a structure containing E,
            // then E can get destroyed during the operation, including E.note_.

            // By constructing a temporary here we
            // * increment the reference of E.exp_ptr.
            // * copy E.note_, so that we can use it safely.

            auto tmp(E);

            std::swap(*this, tmp);

            return *this;
        }

        Exp<NoteV,NoteE>& operator=(Exp<NoteV,NoteE>&& E) noexcept = default;

        template <class Archive>
        void serialize(Archive& ar);

	Exp() = default;
	Exp(const Exp<NoteV,NoteE>& e) = default;
	Exp(Exp<NoteV,NoteE>&& e) noexcept = default;

	Exp(const Var<NoteV>& v):exp_ptr( make_cow_ptr<Var<NoteV>>(v) ) {}
	Exp(const Var<NoteV>& v, const NoteE& n):exp_ptr( make_cow_ptr<Var<NoteV>>(v) ), note_(n) {}
	Exp(const Lambda<NoteV,NoteE>& l):exp_ptr( make_cow_ptr<Lambda<NoteV,NoteE>>(l) ) {}
	Exp(const Lambda<NoteV,NoteE>& l, const NoteE& n):exp_ptr( make_cow_ptr<Lambda<NoteV,NoteE>>(l) ), note_(n) {}
	Exp(const Apply<NoteV,NoteE>& a):exp_ptr( make_cow_ptr<Apply<NoteV,NoteE>>(a) ) {}
	Exp(const Apply<NoteV,NoteE>& a, const NoteE& n):exp_ptr( make_cow_ptr<Apply<NoteV,NoteE>>(a) ), note_(n) {}
	Exp(const Let<NoteV,NoteE>& l):exp_ptr( make_cow_ptr<Let<NoteV,NoteE>>(l) ) {}
	Exp(const Let<NoteV,NoteE>& l, const NoteE& n):exp_ptr( make_cow_ptr<Let<NoteV,NoteE>>(l) ), note_(n) {}
	Exp(const Case<NoteV,NoteE>& c):exp_ptr( make_cow_ptr<Case<NoteV,NoteE>>(c) ) {}
	Exp(const Case<NoteV,NoteE>& c, const NoteE& n):exp_ptr( make_cow_ptr<Case<NoteV,NoteE>>(c) ), note_(n) {}
	Exp(const ConApp<NoteV>& c):exp_ptr( make_cow_ptr<ConApp<NoteV>>(c) ) {}
	Exp(const ConApp<NoteV>& c, const NoteE& n):exp_ptr( make_cow_ptr<ConApp<NoteV>>(c)), note_(n) {}
	Exp(const BuiltinOp<NoteV>& b):exp_ptr( make_cow_ptr<BuiltinOp<NoteV>>(b) ) {}
	Exp(const BuiltinOp<NoteV>& b, const NoteE& n):exp_ptr( make_cow_ptr<BuiltinOp<NoteV>>(b) ), note_(n) {}
	Exp(const Constant& c):exp_ptr( make_cow_ptr<Constant>(c) ) {}
	Exp(const Constant& c, const NoteE& n):exp_ptr( make_cow_ptr<Constant>(c) ), note_(n) {}

	~Exp() = default;
    };

    template <typename NoteV, typename NoteE>
    inline std::ostream& operator<<(std::ostream& o,const Core2::Exp<NoteV,NoteE>& E)
    {
	return o<<E.print();
    }


    template <typename NoteV = std::monostate>
    struct Var
    {
	std::string name;
	int index=0;

	NoteV info;

        // Perhaps this should go on a note...
        bool is_exported = false;

	bool operator==(const Var<NoteV>& v2) const
	{
	    return index == v2.index and name == v2.name;
	}

	std::strong_ordering operator<=>(const Var<NoteV>& v2) const
	{
	    auto cmp1 = index <=> v2.index;

	    if (cmp1 != std::strong_ordering::equivalent) return cmp1;

	    return (name <=> v2.name);
	}

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(name, index, info, is_exported);
	}

        Var() = default;
        Var(const std::string& s):name(s) {}
        Var(const std::string& s, int i):name(s), index(i) {}
        Var(const std::string& s, int i, const NoteV& n):name(s), index(i), info(n) {}
        Var(const std::string& s, int i, const NoteV& n, bool b):name(s), index(i), info(n), is_exported(b) {}
    };

    template <typename NoteV>
    inline std::ostream& operator<<(std::ostream& o,const Core2::Var<NoteV>& V)
    {
	return o<<V.print();
    }


    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Lambda
    {
	Var<NoteV> x;
	Exp<NoteV, NoteE> body;

	std::string print() const;

	bool operator==(const Lambda<NoteV,NoteE>& v2) const
	{
	    return x == v2.x and body == v2.body;
	}

	std::strong_ordering operator<=>(const Lambda<NoteV,NoteE>& v2) const
	{
	    auto cmp1 = x <=> v2.x;
	    if (cmp1 != std::strong_ordering::equivalent) return  cmp1;

	    return body <=> v2.body;
	}

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(x, body);
	}
    };

    // FIXME: Change this to a single arg.
    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Apply
    {
	Exp<NoteV, NoteE> head;
	Var<NoteV> arg;

	std::string print() const;

	bool operator==(const Apply<NoteV,NoteE>& a2) const
	{
	    return head == a2.head and arg == a2.arg;
	}

	std::strong_ordering operator<=>(const Apply<NoteV,NoteE>& a2) const
	{
	    auto cmp = head <=> a2.head;

	    if (cmp != std::strong_ordering::equivalent) return cmp;

	    return arg <=> a2.arg;
	}

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(head, arg);
	}

        Apply() = default;

        Apply(const Exp<NoteV,NoteE>& h, const Var<NoteV>& a)
            :head(h), arg(a)
        {
        }
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Decl
    {
	Var<NoteV> x;
	Exp<NoteV, NoteE> body;

	std::string print() const;

	bool operator==(const Decl<NoteV,NoteE>& decl) const
	{
	    return x == decl.x and body == decl.body;
	}

	std::strong_ordering operator<=>(const Decl<NoteV,NoteE>& decl) const
	{
	    auto cmp = x <=> decl.x;

	    if (cmp != std::strong_ordering::equivalent) return cmp;

	    return body <=> decl.body;
	}

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(x, body);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Decls: public std::vector<Decl<NoteV,NoteE>>
    {
	std::string print() const;

	using std::vector<Decl<NoteV,NoteE>>::vector;
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Let
    {
	Decls<NoteV,NoteE> decls;
	Exp<NoteV,NoteE> body;

	bool operator==(const Let<NoteV,NoteE>& let) const
	{
	    return decls == let.decls and body == let.body;
	}

	std::strong_ordering operator<=>(const Let<NoteV,NoteE>& let) const
	{
	    auto cmp = decls <=> let.decls;

	    if (cmp != std::strong_ordering::equivalent) return cmp;

	    return body <=> let.body;
	}

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(decls, body);
	}

        Let() = default;
        Let(const Decls<NoteV,NoteE>& d, const Exp<NoteV,NoteE>& e):decls(d), body(e) { assert(not decls.empty()); }
    };

    // Deduction guide.
    template <typename NoteV, typename NoteE, typename T>
    Let(const Decls<NoteV,NoteE>&, const T&) -> Let<NoteV,NoteE>;

    // Deduction guide.
    template <typename NoteV, typename NoteE, typename T>
    Let(const T&, const Exp<NoteV,NoteE>&) -> Let<NoteV,NoteE>;

    template <typename NoteV = std::monostate>
    struct Pattern
    {
        std::optional<std::string> head;
	std::vector<Var<NoteV>> args;

	std::string print() const;

	std::strong_ordering operator<=>(const Pattern<NoteV>&) const = default;
	bool operator==(const Pattern<NoteV>& v2) const = default;

	bool is_wildcard_pat() const
	{
            return not head;
	}

        bool is_con_pat() const
        {
            return head.has_value();
        }

	bool is_irrefutable() const
	{
            return is_wildcard_pat();
	}

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(head, args);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Alt
    {
	Pattern<NoteV> pat;
	Exp<NoteV,NoteE> body;

	std::string print() const;

	bool operator==(const Alt<NoteV,NoteE>& v2) const
	{
	    return pat == v2.pat and body == v2.body;
	}

	std::strong_ordering operator<=>(const Alt<NoteV,NoteE>& v2) const
	{
	    auto cmp1 = pat <=> v2.pat;

	    if (cmp1 != std::strong_ordering::equivalent) return cmp1;

	    return body <=> v2.body;
	}

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(pat, body);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Case
    {
	Exp<NoteV, NoteE> object;
        std::vector<Alt<NoteV, NoteE>> alts;

	bool operator==(const Case<NoteV,NoteE>& v2) const
	{
	    return object == v2.object and alts == v2.alts;
	}

	std::strong_ordering operator<=>(const Case<NoteV,NoteE>& v2) const
	{
	    auto cmp1 = object <=> v2.object;

	    if (cmp1 != std::strong_ordering::equivalent) return cmp1;

	    return alts <=> v2.alts;
	}

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(object, alts);
	}
    };
    
    template <typename NoteV = std::monostate>
    struct ConApp
    {
	std::string head;
	std::vector<Var<NoteV>> args;

	std::string print() const;

	bool operator==(const ConApp<NoteV>& v2) const
	{
	    return head == v2.head and args == v2.args;
	}

	std::strong_ordering operator<=>(const ConApp<NoteV>& v2) const
	{
	    auto cmp1 = head <=> v2.head;

	    if (cmp1 != std::strong_ordering::equivalent) return cmp1;

	    return args <=> v2.args;
	}

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(head, args);
	}

        ConApp() = default;
        ConApp(const std::string& h, const std::vector<Var<NoteV>>& a):head(h),args(a) {}
    };

    template <typename NoteV = std::monostate>
    struct BuiltinOp
    {
	std::string lib_name;
	std::string func_name;
	std::vector<Var<NoteV>> args;
	void* op = nullptr;

	std::string print() const;

	bool operator==(const BuiltinOp<NoteV>& v2) const
	{
	    return lib_name == v2.lib_name and func_name == v2.func_name and args == v2.args;
	}

	std::strong_ordering operator<=>(const BuiltinOp<NoteV>& v2) const
	{
	    auto cmp1 = lib_name <=> v2.lib_name;

	    if (cmp1 != std::strong_ordering::equivalent) return cmp1;

	    auto cmp2 = func_name <=> v2.func_name;

	    if (cmp2 != std::strong_ordering::equivalent) return cmp2;

	    return args <=> v2.args;
	}

	template <class Archive>
	void load(Archive& ar)
	{
	    ar(lib_name, func_name, args);
	    op = nullptr;
	}

	template <class Archive>
	void save(Archive& ar) const
	{
	    ar(lib_name, func_name, args);
	}
    };

    struct Constant
    {
	std::variant<char,int,integer_container,std::string> value;

	std::string print() const;

	std::strong_ordering operator<=>(const Constant& v2) const = default;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(value);
	}
    };

    template <typename NoteV, typename NoteE>
    template <class Archive>
    void Exp<NoteV,NoteE>::serialize(Archive& ar)
    {
	ar(exp_ptr, note_);
    }

    template <typename NoteV, typename NoteE>
    std::string Exp<NoteV,NoteE>::print() const
    {
        if (exp_ptr.valueless_by_exception()) return "INVALID";
	if (empty()) return "NOEXP";

	if (auto v = to_var()) return v->print();
	else if (auto lam = to_lambda()) return lam->print();
	else if (auto a = to_apply()) return a->print();
	else if (auto let = to_let()) return let->print();
	else if (auto c = to_case()) return c->print();
	else if (auto con = to_conApp()) return con->print();
	else if (auto b = to_builtinOp()) return b->print();
	else if (auto constant = to_constant()) return constant->print();
	else std::abort();
    }

    template <typename NoteV>
    std::string Var<NoteV>::print() const
    {
	if (is_haskell_qsym(name)) // (:) or QVARSYM or QCONSYM
	{
	    assert(index == 0);
	    return std::string("(") + name + ")";
	}

	std::string s = name;
	if (name.empty() or index != 0)
	    s += std::string("#")+std::to_string(index);

	return s;
    }

    template <typename NoteV, typename NoteE>
    std::string Lambda<NoteV,NoteE>::print() const
    {
	std::vector<std::string> svars;
	svars.push_back(x.print());
	auto body2 = body;
	while(auto l = body2.to_lambda())
	{
	    svars.push_back(l->x.print());
	    body2 = l->body;
	}
	return "\\"+join(svars," ") + " -> " + body2.print();
    }

    template <typename NoteV, typename NoteE>
    std::string Apply<NoteV,NoteE>::print() const
    {
        std::string result = head.print();
        if (not head.to_var() and not head.to_apply())
            result = "(" + result + ")";
        result += " " + arg.print();
	return result;
    }

    template <typename NoteV, typename NoteE>
    std::string Decl<NoteV,NoteE>::print() const
    {
	return x.print() + " = " + body.print();
    }

    template <typename NoteV, typename NoteE>
    std::string Decls<NoteV,NoteE>::print() const
    {
	std::vector<std::string> decls;
	for(auto& decl: *this)
	    decls.push_back(decl.print());
	return "{"+join(decls,"; ")+"}";
    }

    template <typename NoteV, typename NoteE>
    std::string Let<NoteV,NoteE>::print() const
    {
	return "let " + decls.print() + " in " + body.print();
    }

    template <typename NoteV>
    std::string Pattern<NoteV>::print() const
    {
        if (not head)
            return "_";
        else
        {
            std::vector<std::string> words;
            words.push_back(*head);
            for(auto& v: args)
                words.push_back(v.print());

            if (words.size() == 3 and words[0].starts_with(":"))
                return words[1]+words[0]+words[2];

            if (is_tuple_name(words[0]) and tuple_arity(words[0])+1 == words.size())
            {
                words.erase(words.begin());
                return "("+join(words,", ")+")";
            }

            return join(words," ");
        }
    }

    template <typename NoteV, typename NoteE>
    std::string Alt<NoteV,NoteE>::print() const
    {
	return pat.print() + " -> " + body.print();
    }

    template <typename NoteV, typename NoteE>
    std::string Case<NoteV,NoteE>::print() const
    {
	std::vector<std::string> s_alts;
	for(auto& alt: alts)
	    s_alts.push_back(alt.print());
	return "case " + object.print() + " of {"+join(s_alts,"; ")+"}";
    }

    template <typename NoteV>
    std::string ConApp<NoteV>::print() const
    {
	std::vector<std::string> words;
	words.push_back(head);
	for(auto& v: args)
	    words.push_back(v.print());

	if (words.size() == 3 and words[0].starts_with(":"))
	    return words[1]+words[0]+words[2];

	if (is_tuple_name(words[0]) and tuple_arity(words[0])+1 == words.size())
	{
	    words.erase(words.begin());
	    return "("+join(words,", ")+")";
	}

	return join(words," ");
    }

    template <typename NoteV>
    std::string BuiltinOp<NoteV>::print() const
    {
	std::vector<std::string> words;
	words.push_back(lib_name + ":" + func_name);
	for(auto& v: args)
	    words.push_back(v.print());
	return join(words," ");
    }

    inline std::string Constant::print() const
    {
	if (std::holds_alternative<char>(value))
	    return (std::string("'") + std::get<char>(value) + "'");
	else if (std::holds_alternative<int>(value))
	    return std::to_string(std::get<int>(value));
	else if (std::holds_alternative<integer_container>(value))
	    return std::get<integer_container>(value).i.str();
	else if (std::holds_alternative<std::string>(value))
	    return (std::string("\"") + std::get<std::string>(value) + "\"");
	else
	    std::abort();
    }
}


namespace std
{
    template <typename NoteV>
    class hash < Core2::Var<NoteV> >{
    public :
        size_t operator()(const Core2::Var<NoteV> &x) const
        {
            size_t h =   std::hash<int>()(x.index) ^ std::hash<std::string>()(x.name);
            return  h ;
        }
    };

};

#endif
