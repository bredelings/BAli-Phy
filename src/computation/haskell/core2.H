#ifndef CORE2_H
#define CORE2_H

#include <variant>
#include <vector>
#include <string>
#include "util/cow-ptr.H"
#include "util/string/join.H"
#include "util/math/log-double.H"
#include "computation/haskell/ids.H"
#include "computation/haskell/Integer.H"

#include <cereal/types/memory.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/tuple.hpp>
#include <cereal/types/variant.hpp>
#include <cereal/types/vector.hpp>

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

	std::string print() const;

	bool empty() const
	{
            return std::holds_alternative<std::monostate>(exp_ptr);
	}

	const Var<NoteV>* to_var() const
	{
	    if (exp_ptr.index() == 1)
		return std::get<cow_ptr<Var<NoteV>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Var<NoteV>* to_var_modify()
	{
	    if (exp_ptr.index() == 1)
		return std::get<cow_ptr<Var<NoteV>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Lambda<NoteV,NoteE>* to_lambda() const
	{
	    if (exp_ptr.index() == 2)
		return std::get<cow_ptr<Lambda<NoteV,NoteE>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Lambda<NoteV,NoteE>* to_lambda_modify()
	{
	    if (exp_ptr.index() == 2)
		return std::get<cow_ptr<Lambda<NoteV,NoteE>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Apply<NoteV,NoteE>* to_apply() const
	{
	    if (exp_ptr.index() == 3)
		return std::get<cow_ptr<Apply<NoteV,NoteE>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Apply<NoteV,NoteE>* to_apply_modify()
	{
	    if (exp_ptr.index() == 3)
		return std::get<cow_ptr<Apply<NoteV,NoteE>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Let<NoteV,NoteE>* to_let() const
	{
	    if (exp_ptr.index() == 4)
		return std::get<cow_ptr<Let<NoteV,NoteE>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Let<NoteV,NoteE>* to_let_modify()
	{
	    if (exp_ptr.index() == 4)
		return std::get<cow_ptr<Let<NoteV,NoteE>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Case<NoteV,NoteE>* to_case() const
	{
	    if (exp_ptr.index() == 5)
		return std::get<cow_ptr<Case<NoteV,NoteE>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Case<NoteV,NoteE>* to_case_modify()
	{
	    if (exp_ptr.index() == 5)
		return std::get<cow_ptr<Case<NoteV,NoteE>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const ConApp<NoteV>* to_conApp() const
	{
	    if (exp_ptr.index() == 6)
		return std::get<cow_ptr<ConApp<NoteV>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	ConApp<NoteV>* to_conApp_modify()
	{
	    if (exp_ptr.index() == 6)
		return std::get<cow_ptr<ConApp<NoteV>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const BuiltinOp<NoteV>* to_builtinOp() const
	{
	    if (exp_ptr.index() == 7)
		return std::get<cow_ptr<BuiltinOp<NoteV>>>(exp_ptr).get();
	    else
		return nullptr;
	}

	BuiltinOp<NoteV>* to_builtinOp_modify()
	{
	    if (exp_ptr.index() == 7)
		return std::get<cow_ptr<BuiltinOp<NoteV>>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	const Constant* to_constant() const
	{
	    if (exp_ptr.index() == 8)
		return std::get<cow_ptr<Constant>>(exp_ptr).get();
	    else
		return nullptr;
	}

	Constant* to_constant_modify()
	{
	    if (exp_ptr.index() == 8)
		return std::get<cow_ptr<Constant>>(exp_ptr).modify_ptr().get();
	    else
		return nullptr;
	}

	template <class Archive>
	void serialize(Archive& ar);

	Exp() = default;
	Exp(const Exp<NoteV,NoteE>& e) = default;

	template <typename E>
	Exp(const E& e):exp_ptr( make_cow_ptr<E>(e) ) {}
	
	~Exp() = default;
    };


    template <typename NoteV = std::monostate>
    struct Var
    {
	std::string name;
	int index;

	NoteV info;

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(name, index, info);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Lambda
    {
	std::vector<Var<NoteV>> vars;
	Exp<NoteV, NoteE> body;

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(vars, body);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Apply
    {
	Exp<NoteV, NoteE> head;
	std::vector<Var<NoteV>> args;

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(head, args);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Decl
    {
	Var<NoteV> x;
	Exp<NoteE> body;

	std::string print() const;

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

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(decls, body);
	}
    };

    struct WildcardPat
    {
	std::string print() const;

	template <class Archive>
	void serialize(Archive&)
	{ }
    };

    template <typename NoteV = std::monostate>
    struct ConPat
    {
	std::string head;
	std::vector<Var<NoteV>> args;

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(head, args);
	}
    };

    template <typename NoteV = std::monostate>
    struct VarPat
    {
	Var<NoteV> var;

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(var);
	}
    };
    
    template <typename NoteV = std::monostate>
    struct Pattern: public std::variant<std::monostate, WildcardPat, VarPat<NoteV>, ConPat<NoteV>>
    {
	std::string print() const;

	using std::variant<std::monostate, WildcardPat, VarPat<NoteV>, ConPat<NoteV>>::variant;
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Alt
    {
	Pattern<NoteV> pat;
	Exp<NoteV,NoteE> body;

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(pat, body);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Alts: public std::vector<Alt<NoteV, NoteE>>
    {
	std::string print() const;

	using std::vector<Alt<NoteV,NoteE>>::vector;
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Case
    {
	Exp<NoteV, NoteE> object;
	Alts<NoteV, NoteE> alts;

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

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(head, args);
	}
    };

    template <typename NoteV = std::monostate>
    struct BuiltinOp
    {
	std::string lib_name;
	std::string func_name;
	std::vector<Var<NoteV>> args;

	std::string print() const;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(lib_name, func_name, args);
	}
    };

    struct Constant
    {
	std::variant<char,int,integer_container,std::string> value;

	std::string print() const;

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
	for(auto& x: vars)
	    svars.push_back(x.print());
	return "\\"+join(svars," ") + " -> " + body.print();
    }

    template <typename NoteV, typename NoteE>
    std::string Apply<NoteV,NoteE>::print() const
    {
	std::vector<std::string> words;
	words.push_back("("+head.print()+")");
	for(auto& v: args)
	    words.push_back(v.print());
	return join(words," ");
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

    inline std::string WildcardPat::print() const
    {
	return "_";
    }

    template <typename NoteV>
    std::string ConPat<NoteV>::print() const
    {
	std::vector<std::string> words;
	words.push_back(head);
	for(auto& v: args)
	    words.push_back(v.print());
	return join(words," ");
    }

    template <typename NoteV>
    std::string VarPat<NoteV>::print() const
    {
	return var.print();
    }

    template <typename NoteV>
    std::string Pattern<NoteV>::print() const
    {
	if (this->index() == 0) return "NOPAT";
	else if (this->index() == 1) return std::get<WildcardPat>(*this).print();
	else if (this->index() == 2) return std::get<VarPat<NoteV>>(*this).print();
	else if (this->index() == 3) return std::get<ConPat<NoteV>>(*this).print();
	else std::abort();
    }

    template <typename NoteV, typename NoteE>
    std::string Alt<NoteV,NoteE>::print() const
    {
	return pat.print() + " -> " + body.print();
    }

    template <typename NoteV, typename NoteE>
    std::string Alts<NoteV,NoteE>::print() const
    {
	std::vector<std::string> alts;
	for(auto& alt: *this)
	    alts.push_back(alt.print());
	return "{"+join(alts,";")+"}";
    }

    template <typename NoteV, typename NoteE>
    std::string Case<NoteV,NoteE>::print() const
    {
	return "case " + object.print() + " of " + alts.print();
    }

    template <typename NoteV>
    std::string ConApp<NoteV>::print() const
    {
	std::vector<std::string> words;
	words.push_back(head);
	for(auto& v: args)
	    words.push_back(v.print());
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

#endif
