#include "haskell.H"
#include "util/string/join.H"

using std::string;
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

string show_instance_header(const Context& context, const string& name, const vector<Type>& type_args)
{
    vector<string> ss;
    if (context.constraints.size())
        ss = {context.print(), "=>"};
    ss.push_back(name);
    for(auto& type_arg: type_args)
        ss.push_back(parenthesize_type(type_arg));
    return join(ss, " ");
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

string TypeDecl::print() const
{
    vector<string> var_strings;
    for(auto& var: vars)
        var_strings.push_back(unloc(var.name));

    return join(var_strings,", ") + " :: " + type.print();
}

string StrictValueDecl::print() const
{
    string result = "! " + lhs.print() + " ";
    // This happens for desugared decls which are just var = body
    if (not rhs.is_a<MultiGuardedRHS>())
        result += "= ";
    result += rhs.print();
    return result;
}

string ValueDecl::print() const
{
    string result = lhs.print() + " ";
    // This happens for desugared decls which are just var = body
    if (not rhs.is_a<MultiGuardedRHS>())
        result += "= ";
    result += rhs.print();
    return result;
}

bool ValueDecl::operator==(const Object& O) const
{
    if (this == &O) return true;

    auto vd = dynamic_cast<const ValueDecl*>(&O);

    if (vd)
        return operator==(*vd);
    else
        return false;
}

bool ValueDecl::operator==(const ValueDecl& V) const
{
    return (lhs == V.lhs) and (rhs == V.rhs);
}

string Decls::print() const
{
    vector<string> decl_string;
    for(auto& decl: *this)
        decl_string.push_back( decl.print() );

    return "{"+join( decl_string, "\n;" ) + "\n}";
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
    return unloc(name) == unloc(v.name) and index == v.index;
}

bool Var::operator<(const Var& v) const
{
    if (index < v.index) return true;
    if (index > v.index) return false;

    int cmp = unloc(name).compare(unloc(v.name));

    return (cmp < 0);
}

string TypeVar::print() const
{
    return unloc(name);
}

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
    return unloc(name) == unloc(tv.name) and index == tv.index;
}

bool TypeVar::operator<(const TypeVar& tv) const
{
    if (index < tv.index) return true;
    if (index > tv.index) return false;

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
    return unloc(name) == unloc(tv.name) and index == tv.index;
}

bool TypeCon::operator<(const TypeCon& tv) const
{
    if (index < tv.index) return true;
    if (index > tv.index) return false;

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

string ForallType::print() const
{
    vector<string> binders;
    for(auto& type_var_binder: type_var_binders)
        binders.push_back(type_var_binder.print_with_kind());
    return "forall "+join(binders," ")+". "+type.print();
}

string ConstrainedType::print() const
{
    return context.print() + " => " + type.print();
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
    return pattern.print() + " -> " + rhs.print();
}

string Alts::print() const
{
    vector<string> alt_string;
    for(auto& alt: *this)
        alt_string.push_back(alt.print());
    return "{" + join(alt_string, "\n;") + "\n}";
}

string CaseExp::print() const
{
    return "case " + object.print() + " of " + alts.print();
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
    if (decls)
        result += " where " + decls->print();
    return result;
}

string InstanceDecl::print() const
{
    string result = "instance " + show_instance_header(context, name, type_args);
    if (decls)
        result += " where " + decls->print();
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

std::string MultiGuardedRHS::print() const
{
    string result = "= ";
    for(auto& guarded_rhs: guarded_rhss)
        result += guarded_rhs.print() + "\n";

    if (decls)
        result = result + "where " + unloc(*decls).print();
    return result;
}

MultiGuardedRHS SimpleRHS(const Located<expression_ref>& body, const optional<Located<Decls>>& decls)
{
    return MultiGuardedRHS( {{{},unloc(body)}}   ,decls);
}

std::pair<Type,std::vector<Type>> decompose_type_apps(Type t)
{
    std::vector<Type> args;
    while(t.is_a<TypeApp>())
    {
        auto A = t.as_<TypeApp>();
        args.push_back(A.arg);
        t = A.head;
    }
    std::reverse(args.begin(), args.end());
    return {t,args};
}

string LambdaExp::print() const
{
    string result = "\\";
    for(auto& arg: args)
        result += parenthesize_pattern(arg) + " ";
    result += "-> ";
    result += body.print();
    return result;
}

string LetExp::print() const
{
    vector<expression_ref> decl_string;
    for(auto& decl: unloc(decls))
        decl_string.push_back( decl.print() );

    return "let { " + join( decl_string, "; " ) + " } in " + body.print();
}

string IfExp::print() const
{
    return "if " + condition.print() + " then " + true_branch.print() + " else " + false_branch.print();
}


Type make_arrow_type(const Type& t1, const Type& t2)
{
    static Haskell::TypeCon type_arrow(Located<string>({},"->"));
    return Haskell::TypeApp(Haskell::TypeApp(type_arrow,t1),t2);
}

}
