#include "haskell.H"
#include "util/string/join.H"
#include "expression/tuple.H" // for tuple_name
#include "util/set.H"   // for includes( , )

using std::string;
using std::pair;
using std::vector;
using std::optional;

namespace Haskell
{

string parenthesize_type(const expression_ref& t)
{
    if (t.is_a<TypeCon>() or t.is_a<TypeVar>() or t.is_a<TupleType>() or t.is_a<ListType>())
        return t.print();
    else
        return "(" + t.print() + ")";
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
        var_strings.push_back(unloc(var.name));

    return join(var_strings,", ") + " :: " + type.print();
}

string StrictValueDecl::print() const
{
    return "! " + lhs.print() + rhs.print();
}

string ValueDecl::print() const
{
    return lhs.print() + rhs.print();
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

string BuiltinDecl::print() const
{
    vector<string> v{"builtin", function_name, std::to_string(n_args), symbol_name, plugin_name};
    return join(v," ");
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

string WildcardPattern::print() const
{
    return "_";
}

string parenthesize_pattern(const Pattern& p)
{
    string result = p.print();
    if (p.is_a<Located<Var>>() or p.is_a<Tuple>() or p.is_a<WildcardPattern>() or p.is_a<LazyPattern>() or p.is_a<AsPattern>() or p.is_a<List>())
        ;
    else
        result = "(" + result + ")";
    return result;
}

string LazyPattern::print() const
{
    return "~"+parenthesize_pattern(pattern);
}

string StrictPattern::print() const
{
    return "!"+parenthesize_pattern(pattern);
}

string AsPattern::print() const
{
    return var.print()+"@"+parenthesize_pattern(pattern);
}

string TupleType::print() const
{
    vector<string> parts;
    for(auto& element_type: element_types)
        parts.push_back(element_type.print());
    return "(" + join(parts,", ") +")";
}

Type tuple_type(const std::vector<Type>& ts)
{
    if (ts.size() == 1)
        return ts[0];
    else
        return TupleType(ts);
}

string ListType::print() const
{
    return "[" + element_type.print() + "]";
}

string Con::print() const
{
    return unloc(name);
}

string Var::print() const
{
    return unloc(name);
}

string Var::print_with_type() const
{
    if (type)
        return "("+unloc(name) + " :: " + (*type).print()+")";
    else
        return unloc(name);
}

bool Var::operator==(const Object& o) const
{
    auto V = dynamic_cast<const Var*>(&o);
    if (not V)
        return false;

    return (*this) == *V;
}

bool Var::operator==(const Var& v) const
{
    return unloc(name) == unloc(v.name);
}

bool Var::operator<(const Var& v) const
{
    int cmp = unloc(name).compare(unloc(v.name));

    return cmp;
}

string TypeVar::print() const
{
    return unloc(name);
}

/*
int TypeVar::get_level() const
{
    if (info == typevar_info::other)
        return 0;
    else
        return *level;
}
*/

string TypeVar::print_with_kind() const
{
    if (kind)
        return "("+unloc(name) + " :: " + (*kind).print()+")";
    else
        return unloc(name);
}

bool TypeVar::operator==(const Object& o) const
{
    auto T = dynamic_cast<const TypeVar*>(&o);
    if (not T)
        return false;

    return (*this) == *T;
}

bool TypeVar::operator==(const TypeVar& tv) const
{
    return unloc(name) == unloc(tv.name);
}

bool TypeVar::operator<(const TypeVar& tv) const
{
    int cmp = unloc(name).compare(unloc(tv.name));

    return (cmp < 0);
}

string TypeCon::print() const
{
    return unloc(name);
}

string TypeCon::print_with_kind() const
{
    if (kind)
        return "("+unloc(name) + " :: " + (*kind).print()+")";
    else
        return unloc(name);
}

bool TypeCon::operator==(const Object& o) const
{
    auto T = dynamic_cast<const TypeCon*>(&o);
    if (not T)
        return false;

    return (*this) == *T;
}

bool TypeCon::operator==(const TypeCon& tv) const
{
    return unloc(name) == unloc(tv.name);
}

bool TypeCon::operator<(const TypeCon& tv) const
{
    int cmp = unloc(name).compare(unloc(tv.name));

    return (cmp < 0);
}

string TypeApp::print() const
{
    if (head.is_a<TypeApp>())
    {
        auto& H = head.as_<TypeApp>();
        if (H.head.is_a<TypeCon>())
        {
            auto& A = H.head.as_<TypeCon>();
            if (unloc(A.name) == "->")
                return H.arg.print() + " -> " + arg.print();
        }
    }

    return head.print() + " " + parenthesize_type(arg);
}

Type make_tyapps(const std::vector<Type>& tyapps)
{
    assert(not tyapps.empty());
    Type T = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	T = Haskell::TypeApp(T,tyapps[i]);
    return T;
}

Type make_tyapps(const Type& T0, const std::vector<Type>& args)
{
    Type T = T0;
    for(auto& arg: args)
	T = Haskell::TypeApp(T, arg);
    return T;
}

string ForallType::print() const
{
    vector<string> binders;
    for(auto& type_var_binder: type_var_binders)
        binders.push_back(type_var_binder.print_with_kind());
    return "forall "+join(binders," ")+". "+type.print();
}

Type add_forall_vars(const std::vector<TypeVar>& type_vars, const Type& type)
{
    for(auto& tv: type_vars)
        assert(tv.kind);

    if (type_vars.empty())
        return type;
    else if (auto FAT = type.to<ForallType>())
    {
        auto new_type_vars = type_vars;
        for(auto& type_var: FAT->type_var_binders)
        {
            assert(not includes(type_vars, type_var));
            new_type_vars.push_back(type_var);
        }
        return ForallType(new_type_vars, FAT->type);
    }
    else
        return ForallType(type_vars, type);
}

string ConstrainedType::print() const
{
    return context.print() + " => " + type.print();
}

Type add_constraints(const vector<Type>& constraints, const Type& type)
{
    if (constraints.empty())
        return type;
    else if (type.is_a<ConstrainedType>())
    {
        auto CT = type.as_<ConstrainedType>();
        for(auto& constraint: constraints)
            CT.context.constraints.push_back(constraint);
        return CT;
    }
    else
        return ConstrainedType(Context(constraints),type);
}

Type add_constraints(const Context& context, const Type& type)
{
    return add_constraints(context.constraints, type);
}

std::string Context::print() const
{
    vector<string> cs;
    for(auto& constraint: constraints)
        cs.push_back(constraint.print());

    string result = join(cs,", ");
    if (cs.size() == 1)
        return result;
    else
        return "(" + result + ")";
}

string StrictLazyType::print() const
{
    string mark = (strict_lazy == StrictLazy::strict)?"!":"~";
    return mark + type.print();
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
    return "case " + object.print() + " of " + alts.print();
}

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

std::string LiteralString::print() const
{
    return '"' + data + '"';
}

std::vector<Type> Constructor::get_field_types() const
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

std::string Constructor::print() const
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

bool Constructor::is_record_constructor() const
{
    return fields.index() == 1;
}

int Constructor::arity() const
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
    if (binds)
        result += " where " + binds->print();
    return result;
}

string InstanceDecl::print() const
{
    string result = "instance " + show_instance_header(context, constraint);
    if (binds)
        result += " where " + binds->print();
    return result;
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

std::string DataOrNewtypeDecl::print() const
{
    string result = (data_or_newtype == DataOrNewtype::data) ? "data " : "newtype ";
    result += show_type_or_class_header(context, name, type_vars);
    result += " = ";
    vector<string> cons;
    for(auto& con: constructors)
        cons.push_back(con.print());
    result += join(cons, " | ");
    return result;
}

std::optional<Constructor> DataOrNewtypeDecl::find_constructor_by_name(const string& s) const
{
    for(auto& constructor: constructors)
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
    for(auto& arg: args)
        result += parenthesize_pattern(arg) + " ";
    result += "-> ";
    result += body.print_no_equals();
    return result;
}

LambdaExp::LambdaExp(const std::vector<Pattern>& ps, const expression_ref& b)
    : args(ps), body(SimpleRHS({noloc,b}))
{
    assert(not ps.empty());
}

string LetExp::print() const
{
    return "let " + binds.print() + " in " + body.print();
}

LetExp simple_let(const Var& x, const Expression& E, const Expression& body)
{
    auto decl = simple_decl(x, E);
    Hs::Decls decls({decl});
    Hs::Binds binds({decls});

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
	else if (auto b = decl.to<BuiltinDecl>())
            builtin_decls.push_back(*b);
        else if (decl.is_a<ClassDecl>() or decl.is_a<TypeSynonymDecl>() or decl.is_a<DataOrNewtypeDecl>() or decl.is_a<InstanceDecl>())
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
        ss.push_back(pat.print());
    ss.push_back(rhs.print());

    return join( ss, " ");
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
    s += dict_binds.print()+"\n";
    s += body.print();
    return s;
}

string FunDecl::print() const
{
    vector<string> lines;
    for(auto& rule: match.rules)
        lines.push_back( v.print() + " " + rule.print());

    return join( lines, "\n" );
}

FunDecl simple_fun_decl(const Var& v, const std::vector<Pattern>& pats, const expression_ref& body)
{
    MRule rule{pats, SimpleRHS({noloc,body})};
    Match m{{rule}};

    // v = E
    return FunDecl(v,m);
}

FunDecl simple_decl(const Var& v, const expression_ref& E)
{
    return simple_fun_decl(v,{},E);
}

}

