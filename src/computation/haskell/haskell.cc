#include "haskell.H"
#include "ids.H"
#include "util/string/join.H"
#include "util/set.H"           // for includes( , )
#include "util/variant.H"       // for to<>()
#include "util/string/split.H"  // for split( , )
#include "typecheck/kind.H"

using std::string;
using std::pair;
using std::tuple;
using std::vector;
using std::optional;

namespace Haskell
{

std::tuple<LExp, vector<LExp>> decompose_apps(const LExp& E)
{
    if (auto app = unloc(E).to<ApplyExp>())
    {
        auto head_args = decompose_apps(app->head);
        std::get<1>(head_args).push_back(app->arg);
        return head_args;
    }
    else
        return {E,{}};
}

vector<LExp> flatten(const LExp& E)
{
    if (auto app = unloc(E).to<ApplyExp>())
    {
        auto terms = flatten(app->head);
        terms.push_back(app->arg);
        return terms;
    }
    else
        return {E};
}

LExp apply(const std::vector<LExp>& terms)
{
    auto exp = terms[0];
    for(int i=1;i<terms.size();i++)
    {
        auto& arg = terms[i];

        auto loc = exp.loc * arg.loc;
        exp = {loc, ApplyExp(exp, arg)};
    }
    return exp;
}

LExp apply(const LExp& head, const std::vector<LExp>& args)
{
    auto exp = head;
    for(auto& arg: args)
    {
        auto loc = exp.loc * arg.loc;
        exp = {loc, ApplyExp(exp, arg)};
    }
    return exp;
}

string show_type_or_class_header(const Context& context, const LTypeCon& con, const vector<LTypeVar>& tvs)
{
    vector<string> ss;
    if (context.size())
        ss = {context.print(), "=>"};
    ss.push_back(unloc(con).name);
    for(auto& tv: tvs)
        ss.push_back(tv.print());
    return join(ss, " ");
}

string show_instance_header(const Context& context, const LType& constraint)
{
    string result = constraint.print();
    if (context.size())
        result = context.print() + " => " + result;
    return result;
}

string ExportSubSpec::print() const
{
    vector<string> ns;

    if (not names)
        ns.push_back("..");
    else
    {
        for(auto& name: *names)
            ns.push_back( unloc(name) ) ;
    }

    return " (" + join(ns, ", ") + ")";
}

string print(ImpExpNs ns)
{
    //  using enum ImpExpNs; -- GCC 10 can't handle this.

    if (ns == ImpExpNs::module) return "module";
    else if (ns == ImpExpNs::type) return "type";
    else if (ns == ImpExpNs::pattern) return "pattern";

    std::abort();
}


string Export::print() const
{
    string result = symbol.print();
    if (ns)
        result = Hs::print(unloc(*ns)) + " " + result;
    if (subspec)
        result += " " + subspec->print();
    return result;
};

bool Export::is_module() const
{
    bool is_mod = (ns and unloc(*ns) == ImpExpNs::module);
    assert(not is_mod or is_haskell_module_name(unloc(symbol)));
    return is_mod;
}

// how about value constructors, with "pattern"?

bool Export::is_value() const
{
    if (ns)
        return false;

    // varid or varsym at end of path
    return is_haskell_var_name(unloc(symbol));
}

bool Export::is_type() const
{
    if (ns)
        return unloc(*ns) == ImpExpNs::type;

    // conid or consym at end of path
    return is_haskell_con_name(unloc(symbol));
}

bool Export::is_default() const
{
    return (ns and unloc(*ns) == ImpExpNs::default_);
}

string ImpSpec::print() const
{
    vector<string> is;
    for(auto& import: imports)
        is.push_back( import.print() );
    string result = "(" + join(is, ", ") + ")";
    if (hiding)
        result = "hiding " + result;
    return result;
}

string ImpDecl::print() const
{
    vector<string> v;
    v.push_back("import");
    if (qualified)
        v.push_back("qualified");
    v.push_back(unloc(modid));
    if (as)
        v.push_back(unloc(*as));
    if (impspec)
        v.push_back(impspec->print());
    return join(v, " ");
}

string Module::print() const
{
    string result = "module " + unloc(modid);

    if (exports)
    {
        vector<string> export_strings;
        for(auto& e: *exports)
            export_strings.push_back(e.print());
        result += "(" + join(export_strings, ", ") + ")";
    }

    result += " where {";

    for(auto& i: impdecls)
        result += i.print() + "; ";

    if (topdecls)
        for(auto& t: *topdecls)
            result += t.print() + "; ";

    result += "}";

    return result;
}

string Neg::print() const
{
    return "-";
}

string InfixExp::print() const
{
    vector<string> ts;
    for(auto& term: terms)
        ts.push_back(unloc(term).print());
    return join(ts, " ");
}

string TypedExp::print() const
{
    return exp.print() + " :: " + type.print();
}

string FixityDecl::print() const
{
    string result;
    if (fixity == Fixity::infixl)
        result = "infixl";
    else if (fixity == Fixity::infixr)
        result = "infixr";
    else if (fixity == Fixity::infix)
        result = "infix";
    else
        result = "fixity:unknown";

    if (precedence)
        result += " " + std::to_string(*precedence);

    result += " " + join(names, ", ");
    return result;
}

string TypeSigDecl::print() const
{
    vector<string> var_strings;
    for(auto& var: vars)
        var_strings.push_back(var.print());

    return join(var_strings,", ") + " :: " + type.print();
}

string KindSigDecl::print() const
{
    vector<string> tycon_strings;
    for(auto& tycon: tycons)
        tycon_strings.push_back(tycon.print());

    return join(tycon_strings,", ") + " :: " + kind.print();
}

string StrictValueDecl::print() const
{
    return "! " + unloc(lhs).print() + rhs.print();
}

string ValueDecl::print() const
{
    return unloc(lhs).print() + rhs.print();
}

bool ValueDecl::operator==(const Object&) const
{
    std::abort();
}

bool ValueDecl::operator==(const ValueDecl&) const
{
    std::abort();
}

std::string InlinePragma::print() const
{
    return "{-# " + command_string.obj + " " + var.obj + " #-}";
}

std::ostream& operator<<(std::ostream& o, inline_pragma_t ip)
{
    if (ip == inline_pragma_t::INLINE)
        o<<"INLINE";
    else if (ip == inline_pragma_t::INLINABLE)
        o<<"INLINABLE";
    else if (ip == inline_pragma_t::NOINLINE)
        o<<"NOINLINE";
    else
        std::abort();
    return o;
}

InlinePragma::InlinePragma(const Located<std::string>& s1, const Located<std::string>& s2)
    :command_string(s1),
     var(s2)
{
    auto [_, cstring] = command_string;

    for(auto& c: cstring)
        c = std::tolower(c);

    if (cstring == "inline")
        command = inline_pragma_t::INLINE;
    else if (cstring == "inlinable" or cstring == "inlineable")
        command = inline_pragma_t::INLINABLE;
    else if (cstring == "noinline" or cstring == "notinline")
        command = inline_pragma_t::NOINLINE;
    else
        throw myexception()<<"Unknown INLINE pragma {-# "<<command_string.obj<<" "<<s2.obj<<" #-}";
}

string Decls::print() const
{
    vector<string> decl_string;

    for(auto& decl: *this)
        decl_string.push_back( decl.print() );

    return "{"+join( decl_string, "\n;" ) + "}";
}

Binds operator+(const Binds& b1, const Binds& b2)
{
    Binds b3 = b1;
    for(auto& x: b2.signatures)
        b3.signatures.insert(x);
    b3.insert(b3.end(), b2.begin(), b2.end());
    return b3;
}

string Binds::print() const
{
    std::ostringstream out;
    for(auto& [var, type]: signatures)
        out<<var.print()<<" :: "<<type.print()<<"\n";
    for(auto& [var, ip]: inline_sigs)
        out<<"{#- "<<ip<<" "<<var.print()<<" #-}\n";
    for(auto& decls: *this)
        out<<decls.print();
    return out.str();
}

string ForeignDecl::print() const
{
    vector<string> v{"foreign", "import", unloc(call_conv), '"'+string(plugin_name)+':'+string(symbol_name)+'"', unloc(function).name, "::", type.print()};
    return join(v," ");
}

ForeignDecl::ForeignDecl(const Located<std::string>& conv, const std::string& n, const Hs::LVar& f, const LType& t)
    : call_conv(conv), function(f), type(t)
{
    vector<string> ns = split(n,":");
    if (ns.size() != 2)
        throw myexception()<<"foreign declaration for "<<f.print()<<": '"<<n<<"' should have exactly one colon";

    plugin_name = ns[0];
    symbol_name = ns[1];

    if (symbol_name.empty())
        symbol_name = unloc(function).name;
}

string FieldBinding::print() const
{
    string result = unloc(field).print();
    if (value)
        result += " = " + unloc(*value).print();
    return result;
}

string FieldBindings::print() const
{
    vector<string> ss;
    for(auto& binding: *this)
        ss.push_back(binding.print());
    if (dotdot)
        ss.push_back("..");
    return join(ss, ",");
}

string RecordExp::print() const
{
    return head.print() + " {" + fbinds.print() + "}";
}

string List::print() const
{
    vector<string> parts;
    for(auto& element: elements)
        parts.push_back(element.print());
    return "[" + join(parts,",") +"]";
}

string ListFrom::print() const
{
    return "[ " + from.print() +" .. ]";
}

string ListFromThen::print() const
{
    return "[ " + from.print() + ", " + then.print() + " .. ]";
}

string ListFromTo::print() const
{
    return "[ " + from.print() +" .. " + to.print() + " ]";
}

string ListFromThenTo::print() const
{
    return "[" + from.print() + ", " + then.print() + " .. " + to.print() + " ]";
}

string ListComprehension::print() const
{
    vector<string> qual_strings;
    for(auto& qual: quals)
        qual_strings.push_back(qual.print());
    return "[" + body.print() + " | " + join(qual_strings,", ") + "]";
}

string LeftSection::print() const
{
    return "("+l_arg.print() + " " + op.print() + ")";
}

string RightSection::print() const
{
    return "("+op.print() + " " +r_arg.print() + ")";
}

string Tuple::print() const
{
    vector<string> parts;
    for(auto& element: elements)
        parts.push_back(element.print());
    return "(" + join(parts,", ") +")";
}

Expression tuple(const std::vector<LExp>& es)
{
    if (es.size() == 1)
        return unloc(es[0]);
    else
        return Tuple(es);
}


string LetQual::print() const
{
    return "let " + binds.print();
}

string SimpleQual::print() const
{
    return exp.print();
}

string PatQual::print() const
{
    return bindpat.print() + " <- " + exp.print();
}

string RecStmt::print() const
{
    return "rec " + stmts.print();
}

string Stmts::print() const
{
    vector<string> stmt_string;
    for(auto& stmt: stmts)
        stmt_string.push_back(stmt.print());
    return "{"+join(stmt_string,"\n;")+"\n}";
}

string Do::print() const
{
    return "do " + stmts.print();
}

string MDo::print() const
{
    return "mdo " + stmts.print();
}

string Alt::print() const
{
    return pattern.print() + rhs.print_no_equals();
}

string Alts::print() const
{
    vector<string> alt_string;
    for(auto& alt: *this)
        alt_string.push_back(alt.print());
    return "{" + join(alt_string, "\n;") + "}";
}

string CaseExp::print() const
{
    vector<string> alt_strings;
    for(auto& alt: alts)
        alt_strings.push_back(alt.patterns[0].print() + alt.rhs.print_no_equals());

    return "case " + unloc(object).print() + " of {" + join(alt_strings, "\n;") + "}";
}

Matches matches_from_alts(const Alts& alts)
{
    Hs::Matches matches;
    for(auto& alt: alts)
    {
        auto& [pattern, body] = unloc(alt);
        matches.push_back(Hs::MRule{{pattern},body});
    }
    return matches;
}

CaseExp::CaseExp(const Located<Expression>& o, const Matches& ms)
    : object(o), alts(ms)
{ }

CaseExp::CaseExp(const Located<Expression>& o, const Alts& as)
    : object(o), alts( matches_from_alts(as) )
{ }

std::string FieldDecl::print() const
{
    vector<string> names;
    for(auto& field_name: field_names)
        names.push_back(field_name.print());

    return join(names, ", ") + " :: " + type.print();
}

std::string FieldDecls::print() const
{
    vector<string> field_decl_strings;
    for(auto& field_decl: field_decls)
        field_decl_strings.push_back(field_decl.print());
    return "{ " + join(field_decl_strings,"; ") + " }";
}

string GADTConstructorDecl::print()  const
{
    vector<string> names;
    for(auto& name: con_names)
        names.push_back(unloc(name));

    return join(names,", ") + " :: " + unloc(type).print();
}

std::vector<LType> ConstructorDecl::get_field_types() const
{
    if (fields.index() == 0)
        return std::get<0>(fields);
    else
    {
        vector<LType> types;
        for(auto& fields: std::get<1>(fields).field_decls)
            for(int i=0;i<fields.field_names.size();i++)
                types.push_back(fields.type);
        return types;
    }
}

string GADTConstructorsDecl::print()  const
{
    vector<string> decls;
    for(auto& data_cons_decl: *this)
        decls.push_back( data_cons_decl.print() );

    return join(decls,"\n");
}

std::string ConstructorDecl::print() const
{
    string result;
    if (forall.size())
    {
        vector<string> var_strings;
        for(auto& var: forall)
            var_strings.push_back(var.print());
        result += "forall " + join(var_strings," ")+".";
    }

    if (not context.empty())
        result += context.print() + " => ";

    result += con->print();

    if (fields.index() == 0)
    {
        for(auto& arg_type: std::get<0>(fields))
            result += " " + parenthesize_type(arg_type, true);
    }
    else
        result += " " + std::get<1>(fields).print();
    return result;
}

bool ConstructorDecl::is_record_constructor() const
{
    return fields.index() == 1;
}

int ConstructorDecl::arity() const
{
    if (is_record_constructor())
    {
        int i = 0;
        for(auto& field_group: std::get<1>(fields).field_decls)
            i += field_group.field_names.size();
        return i;
    }
    else
        return std::get<0>(fields).size();
}

string FunDep::print() const
{
    vector<string> s_lhs;
    for(auto& [loc,tv]: lhs)
        s_lhs.push_back(tv.print());

    vector<string> s_rhs;
    for(auto& [loc,tv]: rhs)
        s_rhs.push_back(tv.print());

    return join(s_lhs,", ") + " -> " + join(s_rhs, ", ");
}

string ClassDecl::print() const
{
    string result = "class " + show_type_or_class_header(context, con, type_vars);
    vector<string> sfun_deps;
    for(auto& fun_dep: fun_deps)
        sfun_deps.push_back(fun_dep.print());
    if (not fun_deps.empty())
        result += " | " + join(sfun_deps,", ");;

    vector<string> decls;
    for(auto& decl: fixity_decls)
        decls.push_back(decl.print());
    for(auto& decl: fam_decls)
        decls.push_back(decl.print());
    for(auto& decl: default_type_inst_decls)
        decls.push_back(decl.print());
    for(auto& decl: sig_decls)
        decls.push_back(decl.print());
    for(auto& decl: default_method_decls)
        decls.push_back(decl.print());

    if (not decls.empty())
        result += " where {" + join(decls,"; ") + "}";

    return result;
}

string InstanceDecl::print() const
{
    string result = "instance " + polytype.print();

    vector<string> decls;
    for(auto& decl: type_inst_decls)
        decls.push_back(decl.print());
    for(auto& decl: method_decls)
        decls.push_back(decl.print());

    if (not decls.empty())
        result += " where {" + join(decls,"; ") + "}";

    return result;
}

std::ostream& operator<<(std::ostream& o, DataOrNewtype d)
{
    if (d == DataOrNewtype::data)
	return (o<<"data");
    else
	return (o<<"newtype");
}


string DataFamilyInstanceDecl::print() const
{
    std::ostringstream out;
    out<<rhs.data_or_newtype<<" instance ";
    out<<con.print();
    for(auto& arg: args)
        out<<" "<<arg.print();
    out<<rhs.print();
    return out.str();
}

string TypeFamilyInstanceEqn::print() const
{
    std::ostringstream out;
    out<<con.print();
    for(auto& arg: args)
        out<<" "<<arg.print();
    out<<" = "<<rhs.print();
    return out.str();
}

string TypeFamilyInstanceDecl::print() const
{
    return "type instance " + TypeFamilyInstanceEqn::print();
}

string FamilyDecl::print() const
{
    std::ostringstream out;
    if (is_type_family())
	out<<"type";
    else
	out<<"data";
    out<<" family "<<con.print();
    for(auto& arg: args)
        out<<" "<<unloc(arg).print_with_kind();
    if (kind_sig)
        out<<" :: "<<kind_sig->print();
    return out.str();
}


vector<Kind> FamilyDecl::arg_kinds() const
{
    vector<Kind> ks;

    for(auto& arg: args)
        ks.push_back(unloc(arg).kind.value_or(kind_type()));

    return ks;
}

bool FamilyDecl::has_kind_notes() const
{
    bool kind_notes = kind_sig.has_value();
    for(auto& tv: args)
        if (unloc(tv).kind)
            kind_notes = true;
    return kind_notes;
}

Kind FamilyDecl::result_kind() const
{
    if (kind_sig)
        return *kind_sig;
    else
        return kind_type();
}

Kind FamilyDecl::kind() const
{
    return function_kind(arg_kinds(), result_kind());
}

string DefaultDecl::print() const
{
    vector<string> ts;
    for(auto& type: types)
        ts.push_back(type.print());

    string result = "default ";
    if (maybe_class)
        result += unloc(*maybe_class) + " ";
    result += "(" + join(ts,", ") + " )";

    return result;
}

string TypeSynonymDecl::print() const
{
    string result = "type " + show_type_or_class_header({}, con, type_vars) + " = " + rhs_type.print();
    return result;
}

string ConstructorsDecl::print() const
{
    vector<string> cons;
    for(auto& con: *this)
        cons.push_back(con.print());
    return join(cons, " | ");
}

bool DataDefn::is_empty_decl() const
{
    return std::holds_alternative<ConstructorsDecl>(constructors);
}

bool DataDefn::is_regular_decl() const
{
    return std::holds_alternative<ConstructorsDecl>(constructors);
}

bool DataDefn::is_gadt_decl() const
{
    return std::holds_alternative<GADTConstructorsDecl>(constructors);
}

const ConstructorsDecl& DataDefn::get_constructors() const
{
    assert(is_regular_decl());
    return std::get<ConstructorsDecl>(constructors);
}

ConstructorsDecl& DataDefn::get_constructors()
{
    assert(is_regular_decl());
    return std::get<ConstructorsDecl>(constructors);
}

const GADTConstructorsDecl& DataDefn::get_gadt_constructors() const
{
    assert(is_gadt_decl());
    return std::get<GADTConstructorsDecl>(constructors);
}

GADTConstructorsDecl& DataDefn::get_gadt_constructors()
{
    assert(is_gadt_decl());
    return std::get<GADTConstructorsDecl>(constructors);
}

std::string DataDefn::print() const
{
    string result;
    if (kind_sig)
        result += " :: " + kind_sig->print();
    
    if (is_regular_decl())
        result += " = " + get_constructors().print();

    else if (is_gadt_decl())
    {
	if (not get_gadt_constructors().empty())
	    result += "\n" + get_gadt_constructors().print();
    }

    return result;
}

std::string DataOrNewtypeDecl::print() const
{
    string result = (data_or_newtype == DataOrNewtype::data) ? "data " : "newtype ";
    result += show_type_or_class_header(context, con, type_vars);
    result += DataDefn::print();

    return result;
}

std::optional<ConstructorDecl> ConstructorsDecl::find_constructor_by_name(const string& s) const
{
    for(auto& constructor: *this)
        if (unloc(*constructor.con).name == s)
            return constructor;
    return {};
}


std::string GuardedRHS::print() const
{
    vector<string> guard_string;
    for(auto& guard: guards)
        guard_string.push_back(unloc(guard).print());
    string result = " = " + unloc(body).print();
    if (not guard_string.empty())
        result = "| " + join(guard_string,", ") + result;
    return result;
}

std::string GuardedRHS::print_no_equals() const
{
    vector<string> guard_string;
    for(auto& guard: guards)
        guard_string.push_back(unloc(guard).print());
    string result = "-> " + unloc(body).print();
    if (not guard_string.empty())
        result = "| " + join(guard_string,", ") + result;
    return result;
}

std::string MultiGuardedRHS::print() const
{
    vector<string> ss;
    for(auto& guarded_rhs: guarded_rhss)
        ss.push_back(guarded_rhs.print());
    if (decls)
        ss.push_back("where " + unloc(*decls).print());

    return join(ss, "\n");
}

std::string MultiGuardedRHS::print_no_equals() const
{
    vector<string> ss;
    for(auto& guarded_rhs: guarded_rhss)
        ss.push_back(guarded_rhs.print_no_equals());
    if (decls)
        ss.push_back("where " + unloc(*decls).print());

    return join(ss, "\n");
}

MultiGuardedRHS SimpleRHS(const Located<expression_ref>& body, const optional<Located<Binds>>& decls)
{
    return MultiGuardedRHS( {{{},body}}   ,decls);
}

string LambdaExp::print() const
{
    string result = "\\";
    for(auto& pat: match.patterns)
        result += parenthesize_pattern(pat) + " ";
    result += match.rhs.print_no_equals();
    return result;
}

LambdaExp::LambdaExp(const LPats& ps, const LExp& b)
    :match(ps, SimpleRHS(b))
{
    assert(not match.patterns.empty());
}

string MatchContext::print() const
{
    if (to<LambdaContext>(*this))
        return "\\";
    else if (to<CaseContext>(*this))
        return "case ";
    else if (auto fc = to<FunctionContext>(*this))
        return get_unqualified_name(fc->name) + " ";
    else
        throw myexception()<<"MatchContext: should be unreachable";
}

std::string parenthesize_exp(const Expression& E)
{
    string s = E.print();
    if (E.is_a<Var>() or E.is_a<Con>() or E.is_a<List>() or E.is_a<Tuple>() or E.is_a<ListComprehension>() or E.is_a<Literal>())
        ;
    else if (E.is_a<WildcardPattern>())
        ;
    else
        s = "(" + s + ")";
    return s;
}

std::string ApplyExp::print() const
{
    auto [head,args] = decompose_apps({noloc,*this});

    string func = unloc(head).print();

    optional<string> op;
    if (auto V = unloc(head).to<Hs::Var>(); V and V->is_sym() and args.size() >= 2)
        op = V->print_without_parens();
    else if (auto C = unloc(head).to<Hs::Con>(); C and C->is_sym() and args.size() >= 2)
        op = C->print();
    if (op)
    {
        string result = parenthesize_exp(unloc(args[0])) + " " + *op + " " + parenthesize_exp(unloc(args[1]));
        if (args.size() > 2)
        {
            result = "(" + result + ")";
            for(int i=2;i<args.size();i++)
                result += " " + parenthesize_exp(unloc(args[i]));
        }
        return result;
    }

    vector<string> ss = {func};
    for(auto& arg: args)
        ss.push_back( parenthesize_exp( unloc(arg) ) );

    return join(ss, " ");
}

ApplyExp::ApplyExp(const LExp& h, const LExp& a)
    :head(h), arg(a)
{
}

string LetExp::print() const
{
    return "let " + binds.print() + " in " + body.print();
}

LetExp simple_let(const LVar& x, const LExp& E, const LExp& body)
{
    auto loc = x.loc * E.loc;
    auto decl = simple_decl(x, E);
    Decls decls({{loc,decl}});
    Binds binds({decls});

    return LetExp({x.loc * E.loc, binds}, body);
}

string IfExp::print() const
{
    return "if " + condition.print() + " then " + true_branch.print() + " else " + false_branch.print();
}

ModuleDecls::ModuleDecls()
{
    value_decls.push_back({});
}

ModuleDecls::ModuleDecls(const Decls& topdecls)
{
    value_decls.push_back({});
    // If instance functions (and presumably default methods) are mutually recursive with value decls,
    // where do we put them?  And how do we handle them?

    // Do we just append different uniques to them and dump them all in the valuedecl pool?
    // I suppose we can separate the instance objects (which contain methods) from the method function definitions.

    for(auto& ldecl: topdecls)
    {
        auto& [loc,decl] = ldecl;

	if (decl.is_a<ValueDecl>() or decl.is_a<TypeSigDecl>() or decl.is_a<FunDecl>() or decl.is_a<PatDecl>())
            value_decls.front().push_back(ldecl);
        else if (auto f = decl.to<FixityDecl>())
        {
            value_decls.front().push_back({loc,decl}); // Fixity decls and split up a collection of value decls for the same function, I think.
            fixity_decls.push_back(*f);
        }
	else if (auto b = decl.to<ForeignDecl>())
            foreign_decls.push_back(*b);
        else if (decl.is_a<ClassDecl>() or decl.is_a<TypeSynonymDecl>() or decl.is_a<DataOrNewtypeDecl>() or decl.is_a<InstanceDecl>())
            type_decls.push_back(ldecl);
        else if (decl.is_a<FamilyDecl>() or decl.is_a<TypeFamilyInstanceDecl>() or decl.is_a<DataFamilyInstanceDecl>())
            type_decls.push_back(ldecl);
        else if (decl.is_a<KindSigDecl>())
            type_decls.push_back(ldecl);
        else if (auto d = decl.to<DefaultDecl>())
            default_decls.push_back({loc,*d});
        else if (auto ip = decl.to<InlinePragma>())
            value_decls.front().push_back({loc,*ip});
        else
            throw myexception()<<"I don't recognize declaration '"<<decl.print()<<"'";
    }
}


string PatDecl::print() const
{
    return lhs.print() + " " + rhs.print();
}

string MRule::print() const
{
    vector<string> ss;
    for(auto& pat: patterns)
        ss.push_back(parenthesize_pattern(pat));
    ss.push_back(rhs.print());

    return join( ss, " ");
}

MRule::MRule(const LPats& ps, const MultiGuardedRHS& r)
    :patterns(ps), rhs(r)
{
}


string GenBind::print() const
{
    vector<string> as;
    for(auto& tv: tv_args)
        as.push_back(tv.print());

    vector<string> ds;
    for(auto& darg: dict_args)
        ds.push_back( darg.print() );

    string s = "[ "+join(as," ")+" ]  [ "+join(ds," ")+" ]";
    for(auto& dd: dict_decls)
        s += dd->print()+"\n";
    s += body.print();
    return s;
}

string FunDecl::print() const
{
    vector<string> lines;
    for(auto& rule: matches)
        lines.push_back( v.print() + " " + rule.print());

    return join( lines, "\n" );
}

FunDecl simple_fun_decl(const LVar& v, const LPats& pats, const LExp& body)
{
    return simple_fun_decl(v, pats, SimpleRHS(body));
}

FunDecl simple_fun_decl(const LVar& v, const LPats& pats, const MultiGuardedRHS& body)
{
    MRule rule{pats, body};
    Matches ms{{rule}};

    // v = E
    return FunDecl(v,ms);
}

FunDecl simple_decl(const LVar& v, const LExp& E)
{
    return simple_fun_decl(v,{},E);
}

FunDecl simple_decl(const LVar& v, const MultiGuardedRHS& E)
{
    return simple_fun_decl(v,{},E);
}

expression_ref error(const std::string& s)
{
    expression_ref error = Var("Compiler.Error.error");
    expression_ref msg = Literal(String{s});
    return {error,msg};
}

Con True()
{
    return {"Data.Bool.True", 0};
}

Con False()
{
    return {"Data.Bool.False", 0};
}

Con ConsCon()
{
    return {":", 2};
}

Con Nil()
{
    return {"[]", 0};
}

Con TupleCon(int n)
{
    return {tuple_name(n), n};
}

std::tuple<std::vector<LTypeVar>, std::vector<LType>, LType> peel_top_gen(LType ltype)
{
    std::vector<LTypeVar> tvs;
    if (auto fa = unloc(ltype).to<ForallType>())
    {
        tvs = fa->type_var_binders;
        ltype = fa->type;
    }

    std::vector<LType> constraints;
    if (auto c = unloc(ltype).to<ConstrainedType>())
    {
        constraints = c->context;
        ltype = c->type;
    }

    return {tvs, constraints, ltype};
}

}
