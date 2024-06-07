#ifndef CORE2_H
#define CORE2_H

#include <variant>
#include <vector>
#include <string>
#include "util/cow-ptr.H"
#include "util/math/log-double.H"

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
    };


    template <typename NoteV = std::monostate>
    struct Var
    {
	std::string name;
	int index;

	NoteV info;
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Lambda
    {
	std::vector<Var<NoteV>> vars;
	Exp<NoteV, NoteE> body;
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Apply
    {
	Exp<NoteV, NoteE> head;
	std::vector<Var<NoteV>> args;
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Decl
    {
	Var<NoteV> x;
	Exp<NoteE> body;
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    using Decls = std::vector<Decl<NoteV,NoteE>>;

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Let
    {
	Decls<NoteV,NoteE> decls;
	Exp<NoteV,NoteE> body;
    };

    struct WildcardPat { };

    template <typename NoteV = std::monostate>
    struct ConPat
    {
	std::string head;
	std::vector<Var<NoteV>> args;
    };

    template <typename NoteV = std::monostate>
    using Pattern = std::variant<WildcardPat, ConPat<NoteV>>;
    
    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Alt
    {
	Pattern<NoteV> pat;
	Exp<NoteV,NoteE> body;
    };

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    using Alts = std::vector<Alt<NoteV, NoteE>>;

    template <typename NoteV = std::monostate, typename NoteE = std::monostate>
    struct Case
    {
	Exp<NoteV, NoteE> object;
	Alts<NoteV, NoteE> alts;
    };
    
    template <typename NoteV = std::monostate>
    struct ConApp
    {
	std::string head;
	std::vector<Var<NoteV>> args;
    };

    template <typename NoteV = std::monostate>
    struct BuiltinOp
    {
	std::tuple<std::string, std::string> op;
	std::vector<Var<NoteV>> args;
    };

    struct Constant
    {
	std::variant<char,int,double,log_double_t,std::string> value;
    };


}

#endif
