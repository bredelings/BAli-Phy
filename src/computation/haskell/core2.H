#ifndef CORE2_H
#define CORE2_H

#include <variant>
#include <vector>
#include <string>
#include "util/cow-ptr.H"
#include "util/math/log-double.H"

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
	    Var<NoteV>,
	    cow_ptr<Lambda<NoteV,NoteE>>,
	    cow_ptr<Apply<NoteV,NoteE>>,
	    cow_ptr<Let<NoteV,NoteE>>,
	    cow_ptr<Case<NoteV,NoteE>>,
	    ConApp<NoteV>,
	    BuiltinOp<NoteV>,
	    Constant
	>
	exp_ptr;

	NoteE note_;

	template <class Archive>
	void serialize(Archive& ar);
    };


    template <typename NoteV = std::monostate>
    struct Var
    {
	std::string name;
	int index;

	NoteV info;

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

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(x, body);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    using Decls = std::vector<Decl<NoteV,NoteE>>;

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Let
    {
	Decls<NoteV,NoteE> decls;
	Exp<NoteV,NoteE> body;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(decls, body);
	}
    };

    struct WildcardPat
    {
	template <class Archive>
	void serialize(Archive&)
	{ }
    };

    template <typename NoteV = std::monostate>
    struct ConPat
    {
	std::string head;
	std::vector<Var<NoteV>> args;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(head, args);
	}
    };

    template <typename NoteV = std::monostate>
    using Pattern = std::variant<WildcardPat, ConPat<NoteV>>;
    
    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Alt
    {
	Pattern<NoteV> pat;
	Exp<NoteV,NoteE> body;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(pat, body);
	}
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    using Alts = std::vector<Alt<NoteV, NoteE>>;

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Case
    {
	Exp<NoteV, NoteE> object;
	Alts<NoteV, NoteE> alts;

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

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(head, args);
	}
    };

    template <typename NoteV = std::monostate>
    struct BuiltinOp
    {
	std::tuple<std::string, std::string> op;
	std::vector<Var<NoteV>> args;

	template <class Archive>
	void serialize(Archive& ar)
	{
	    ar(op, args);
	}
    };

    struct Constant
    {
	std::variant<char,int,double,std::string> value;

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


}

#endif
