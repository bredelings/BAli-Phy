#ifndef CORE2_H
#define CORE2_H

#include <variant>
#include <vector>
#include <string>
#include "util/cow-ptr.H"
#include "util/math/log-double.H"

namespace Core2
{
    struct Var;
    template <typename Note> struct Lambda;
    template <typename Note> struct Apply;
    template <typename Note> struct Let;
    template <typename Note> struct Case;
    struct ConApp;
    struct Constant;
    struct BuiltinOp;

    template <typename Note = std::monostate>
    struct Exp
    {
	std::variant
	<
	    std::monostate,
	    Var,
	    cow_ptr<Lambda<Note>>,
	    cow_ptr<Apply<Note>>,
	    cow_ptr<Let<Note>>,
	    cow_ptr<Case<Note>>,
	    ConApp,
	    Constant,
	    BuiltinOp
	>
	exp_ptr;

	Note note_t;
    };


    struct Var
    {
	std::string name;
    };

    template <typename Note = std::monostate>
    struct Lambda
    {
	std::vector<Var> vars;
	Exp<Note> body;
    };

    template <typename Note = std::monostate>
    struct Apply
    {
	Exp<Note> head;
	std::vector<Var> args;
    };

    template <typename Note = std::monostate>
    struct Decl
    {
	Var x;
	Exp<Note> body;
    };

    template <typename Note = std::monostate>
    using Decls = std::vector<Decl<Note>>;

    template <typename Note = std::monostate>
    struct Let
    {
	Decls<Note> decls;
	Exp<Note> body;
    };

    struct WildcardPat { };

    struct ConPat
    {
	std::string head;
	std::vector<Var> args;
    };

    typedef std::variant<WildcardPat, ConPat> Pattern;
    
    template <typename Note = std::monostate>
    struct Alt
    {
	Pattern pat;
	Exp<Note> body;
    };

    template <typename Note = std::monostate>
    using Alts = std::vector<Alt<Note>>;

    template <typename Note = std::monostate>
    struct Case
    {
	Exp<Note> object;
	Alts<Note> alts;
    };
    
    struct ConApp
    {
	std::string head;
	std::vector<Var> args;
    };

    struct Constant
    {
	std::variant<char,int,double,log_double_t,std::string> value;
    };

    struct BuiltinOp
    {
	std::tuple<std::string, std::string> op;
	std::vector<Var> args;
    };


}

#endif
