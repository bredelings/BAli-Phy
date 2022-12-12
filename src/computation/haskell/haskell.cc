#include "haskell.H"
#include "ids.H"
#include "util/string/join.H"
#include "util/set.H"           // for includes( , )
#include "util/string/split.H"  // for split( , )
#include "typecheck/kind.H"

using std::string;
using std::pair;
using std::vector;
using std::optional;

namespace Haskell
{

void flatten(ApplyExp& App)
{
    // Flatten, in case we have an apply of an apply
    if (auto app2 = App.head.to<Hs::ApplyExp>())
    {
        auto App2 = *app2;

        flatten(App2);

        for(auto& arg: App.args)
            App2.args.push_back( std::move(arg) );

        std::swap(App, App2);
    }
}

string show_type_or_class_header(const Context& context, const string& name, const vector<TypeVar>& tvs)
{
    vector<string> ss;
    if (context.constraints.size())
        ss = {context.print(), "=>"};
    ss.push_back(name);
    for(auto& tv: tvs)
        ss.push_back(tv.print());
    return join(ss, " ");
}

string show_instance_header(const Context& context, const Type& constraint)
{
    string result = constraint.print();
    if (context.constraints.size())
        result = context.print() + " => " + result;
    return result;
}

string ExportSubSpecSome::print() const
{
    vector<string> ns;
    for(auto& name: names)
        ns.push_back( unloc(name) ) ;
    return " (" + join(ns, ", ") + ")";
}

string ExportSubSpecAll::print() const
{
    return "(..)";
}

string ExportSubSpec::print() const
{
    return std::visit([](auto&& x) {return x.print();}, (const std::variant<ExportSubSpecSome,ExportSubSpecAll>&)(*this));
}

string ExportSymbol::print() const
{
    auto result = symbol.print();
    if (subspec)
    {
        result += " " + subspec->print();
    }
    return result;
};

string ExportModule::print() const
{
    return "module " + unloc(modid);
};

string Export::print() const
{
    // FIXME - Cast required to work around bug in GCC 11 libstdc++, supposedly fixed for GCC 12.
    // See https://gitlab.com/jonathan-wakely/gcc/-/commit/486d89e403a18ef78f05f2efb1bc86bbd396899c
    // See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90943
    return std::visit([](auto&& x) {return x.print();}, (const std::variant<ExportSymbol,ExportModule>&)(*this));
};

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
    v.push_back(modid);
    if (as)
        v.push_back(*as);
    if (impspec)
        v.push_back(impspec->print());
    return join(v, " ");
}

string Module::print() const
{
    string result = "module " + modid;

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
        ts.push_back(term.print());
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

string SignatureDecl::print() const
{
    vector<string> var_strings;
    for(auto& var: vars)
        var_strings.push_back(var.print());

    return join(var_strings,", ") + " :: " + type.print();
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
    vector<string> ds;
    for(auto& [name, type]: signatures)
        ds.push_back( name + " :: " + type.print() );
    for(auto& decls: *this)
        ds.push_back(decls.print());
    return join( ds, "\n");
}

string ForeignDecl::print() const
{
    vector<string> v{"foreign", "import", "bpcall", '"'+string(plugin_name)+':'+string(symbol_name)+'"', function_name, "::", type.print()};
    return join(v," ");
}

int ForeignDecl::n_args() const
{
    auto t = type;
    int n = 0;

    while(auto p = is_gen_function_type(t))
    {
        n++;
        auto& [from,to] = *p;
        t = to;
    }

    return n;
}

ForeignDecl::ForeignDecl(const std::string& n, const std::string& o, const Type& t)
    : function_name(o), type(t)
{
    vector<string> ns = split(n,":");
    if (ns.size() != 2)
        throw myexception()<<"foreign declaration for "<<o<<": '"<<n<<"' should have exactly one colon";

    plugin_name = ns[0];
    symbol_name = ns[1];

    if (symbol_name.empty())
        symbol_name = function_name;
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

Expression tuple(const std::vector<Expression>& es)
{
    if (es.size() == 1)
        return es[0];
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
    return pattern.print() + " -> " + rhs.print_no_equals();
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
        alt_strings.push_back(alt.patterns[0].print() + " -> " + alt.rhs.print_no_equals());

    return "case " + object.print() + " of {" + join(alt_strings, "\n;") + "}";
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

CaseExp::CaseExp(const Expression& o, const Matches& ms)
    : object(o), alts(ms)
{ }

CaseExp::CaseExp(const Expression& o, const Alts& as)
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

std::vector<Type> ConstructorDecl::get_field_types() const
{
    if (fields.index() == 0)
        return std::get<0>(fields);
    else
    {
        vector<Type> types;
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
    if (context)
    {
        result += context->print() + " => ";
    }
    result += name;
    if (fields.index() == 0)
    {
        for(auto& arg_type: std::get<0>(fields))
            result += " " + parenthesize_type(arg_type);
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

string ClassDecl::print() const
{
    string result = "class " + show_type_or_class_header(context, name, type_vars);

    vector<string> decls;
    for(auto& decl: fixity_decls)
        decls.push_back(decl.print());
    for(auto& decl: type_fam_decls)
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
    string result = "instance " + show_instance_header(context, constraint);

    vector<string> decls;
    for(auto& decl: type_inst_decls)
        decls.push_back(decl.print());
    for(auto& decl: method_decls)
        decls.push_back(decl.print());

    if (not decls.empty())
        result += " where {" + join(decls,"; ") + "}";

    return result;
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

string TypeFamilyDecl::print() const
{
    std::ostringstream out;
    out<<"type family "<<con.print();
    for(auto& arg: args)
        out<<" "<<arg.print_with_kind();
    if (kind_sig)
        out<<" :: "<<kind_sig->print();
    return out.str();
}


vector<Kind> TypeFamilyDecl::arg_kinds() const
{
    vector<Kind> ks;

    for(auto& arg: args)
        ks.push_back(arg.kind.value_or(kind_type()));

    return ks;
}

Kind TypeFamilyDecl::result_kind() const
{
    if (kind_sig)
        return *kind_sig;
    else
        return kind_type();
}

Kind TypeFamilyDecl::kind() const
{
    return function_kind(arg_kinds(), result_kind());
}

string DefaultDecl::print() const
{
    vector<string> ts;
    for(auto& type: types)
        ts.push_back(type.print());
    return "default (" + join(ts,", ") + " )";
}

string TypeSynonymDecl::print() const
{
    string result = "type " + show_type_or_class_header({}, name, type_vars) + " = " + rhs_type.print();
    return result;
}

string ConstructorsDecl::print() const
{
    vector<string> cons;
    for(auto& con: *this)
        cons.push_back(con.print());
    return join(cons, " | ");
}

bool DataOrNewtypeDecl::is_empty_decl() const
{
    return std::holds_alternative<ConstructorsDecl>(constructors);
}

bool DataOrNewtypeDecl::is_regular_decl() const
{
    return std::holds_alternative<ConstructorsDecl>(constructors);
}

bool DataOrNewtypeDecl::is_gadt_decl() const
{
    return std::holds_alternative<GADTConstructorsDecl>(constructors);
}

const ConstructorsDecl& DataOrNewtypeDecl::get_constructors() const
{
    assert(is_regular_decl());
    return std::get<ConstructorsDecl>(constructors);
}

ConstructorsDecl& DataOrNewtypeDecl::get_constructors()
{
    assert(is_regular_decl());
    return std::get<ConstructorsDecl>(constructors);
}

const GADTConstructorsDecl& DataOrNewtypeDecl::get_gadt_constructors() const
{
    assert(is_gadt_decl());
    return std::get<GADTConstructorsDecl>(constructors);
}

GADTConstructorsDecl& DataOrNewtypeDecl::get_gadt_constructors()
{
    assert(is_gadt_decl());
    return std::get<GADTConstructorsDecl>(constructors);
}

std::string DataOrNewtypeDecl::print() const
{
    string result = (data_or_newtype == DataOrNewtype::data) ? "data " : "newtype ";
    result += show_type_or_class_header(context, name, type_vars);
    if (kind_sig)
        result += " :: " + kind_sig->print();
    
    if (is_regular_decl())
        result += " = " + get_constructors().print();

    else if (is_gadt_decl())
        result += "\n" + get_gadt_constructors().print();

    return result;
}

std::optional<ConstructorDecl> ConstructorsDecl::find_constructor_by_name(const string& s) const
{
    for(auto& constructor: *this)
        if (constructor.name == s)
            return constructor;
    return {};
}


std::string GuardedRHS::print() const
{
    vector<string> guard_string;
    for(auto& guard: guards)
        guard_string.push_back(guard.print());
    string result = " = " + body.print();
    if (not guard_string.empty())
        result = "| " + join(guard_string,", ") + result;
    return result;
}

std::string GuardedRHS::print_no_equals() const
{
    assert(guards.empty());
    return body.print();
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
    assert(guarded_rhss.size() == 1);
    vector<string> ss;
    ss.push_back(guarded_rhss[0].print_no_equals());
    if (decls)
        ss.push_back("where " + unloc(*decls).print());

    return join(ss, "\n");
}

MultiGuardedRHS SimpleRHS(const Located<expression_ref>& body, const optional<Located<Binds>>& decls)
{
    return MultiGuardedRHS( {{{},unloc(body)}}   ,decls);
}

string LambdaExp::print() const
{
    string result = "\\";
    for(auto& pat: match.patterns)
        result += parenthesize_pattern(pat) + " ";
    result += "-> ";
    result += match.rhs.print_no_equals();
    return result;
}

LambdaExp::LambdaExp(const std::vector<Pattern>& ps, const expression_ref& b)
    :match(ps, SimpleRHS({noloc,b}))
{
    assert(not match.patterns.empty());
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
    string func = head.print();

    if (auto V = head.to<Hs::Var>(); V and V->is_sym() and args.size() >= 2)
    {
        string result = parenthesize_exp(args[0]) + " " + V->print_without_parens() + " " + parenthesize_exp(args[1]);
        if (args.size() > 2)
        {
            result = "(" + result + ")";
            for(int i=2;i<args.size();i++)
                result += " " + parenthesize_exp(args[i]);
        }
        return result;
    }

    vector<string> ss = {func};
    for(auto& arg: args)
        ss.push_back( parenthesize_exp( arg ) );

    return join(ss, " ");
}

ApplyExp::ApplyExp(const Expression& h, const std::vector<Expression>& as)
    :head(h), args(as)
{
}

string LetExp::print() const
{
    return "let " + binds.print() + " in " + body.print();
}

LetExp simple_let(const Var& x, const Expression& E, const Expression& body)
{
    auto decl = simple_decl(x, E);
    Decls decls({decl});
    Binds binds({decls});

    return LetExp({noloc, binds}, {noloc,body});
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

    for(auto& decl: topdecls)
    {
	if (decl.is_a<ValueDecl>() or decl.is_a<SignatureDecl>() or decl.is_a<FunDecl>() or decl.is_a<PatDecl>())
            value_decls.front().push_back(decl);
        else if (auto f = decl.to<FixityDecl>())
        {
            value_decls.front().push_back(decl); // Fixity decls and split up a collection of value decls for the same function, I think.
            fixity_decls.push_back(*f);
        }
	else if (auto b = decl.to<ForeignDecl>())
            foreign_decls.push_back(*b);
        else if (decl.is_a<ClassDecl>() or decl.is_a<TypeSynonymDecl>() or decl.is_a<DataOrNewtypeDecl>() or decl.is_a<InstanceDecl>())
            type_decls.push_back(decl);
        else if (decl.is_a<TypeFamilyDecl>() or decl.is_a<TypeFamilyInstanceDecl>())
            type_decls.push_back(decl);
        else if (auto d = decl.to<DefaultDecl>())
        {
            if (default_decl)
                throw myexception()<<"Found more than 1 default declaration in module!";
            else
                default_decl = *d;
        }
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

MRule::MRule(const std::vector<Pattern>& ps, const MultiGuardedRHS& r)
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
        ds.push_back( darg.print_with_type() );

    string s = "[ "+join(as," ")+" ]  [ "+join(ds," ")+" ]";
    s += print_cdecls(*dict_decls)+"\n";
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

FunDecl simple_fun_decl(const Var& v, const std::vector<Pattern>& pats, const expression_ref& body)
{
    return simple_fun_decl(v, pats, SimpleRHS({noloc,body}));
}

FunDecl simple_fun_decl(const Var& v, const std::vector<Pattern>& pats, const MultiGuardedRHS& body)
{
    MRule rule{pats, body};
    Matches ms{{rule}};

    // v = E
    return FunDecl(v,ms);
}

FunDecl simple_decl(const Var& v, const expression_ref& E)
{
    return simple_fun_decl(v,{},E);
}

FunDecl simple_decl(const Var& v, const MultiGuardedRHS& E)
{
    return simple_fun_decl(v,{},E);
}

expression_ref error(const std::string& s)
{
    expression_ref error = Var({noloc,"Compiler.Error.error"});
    expression_ref msg = Literal(String{s});
    return {error,msg};
}

string EvidenceDecls::print()  const
{
    return "core::let " + print_cdecls(*decls) + " in " + body.print();
}

Con True()
{
    return {{noloc, "Data.Bool.True"}, 0};
}

Con False()
{
    return {{noloc, "Data.Bool.False"}, 0};
}

Con ConsCon()
{
    return {{noloc,":"}, 2};
}

Con Nil()
{
    return {{noloc,"[]"}, 0};
}

Con TupleCon(int n)
{
    return {{noloc,tuple_name(n)}, n};
}

}
