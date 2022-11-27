#include "typecheck.H"
#include "kindcheck.H"
#include "haskell/ids.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

Hs::FunDecl dictionary_extractor(const string& name, int i, int N)
{
    // Maybe we should emit the case directly, instead of relying on desugaring?

    // FIXME: We might need to put types on "extractor" and "field".

    Hs::Var field({noloc,"field"});

    // extractor (_,field,_,_) = field;
    Hs::Var extractor({noloc, name});

    // (_,field,_,_)
    vector<Hs::Pattern> pats(N, Hs::WildcardPattern());
    pats[i] = Hs::VarPattern(field);
    Hs::Pattern pattern = Hs::tuple_pattern(pats);

    // (_,field,_,_) -> field
    Hs::MRule rule{{pattern}, Hs::SimpleRHS({noloc, field})};
    Hs::Matches matches{{rule}};

    return Hs::FunDecl(extractor, matches);
}

// OK, so
// * global_value_env    = name         :: forall a: class var => signature (i.e. a-> b -> a)
// * global_instance_env = made-up-name :: forall a: class var => superclass var
// * Hs::Decls           = { name         = \dict -> case dict of (_,_,method,_,_) -> method }
//                       = { made-up-name = \dict -> case dict of (superdict,_,_,_,_) -> superdict }

tuple<global_value_env, ClassInfo, Hs::Decls>
typechecker_state::infer_type_for_class(const Hs::ClassDecl& class_decl)
{
    kindchecker_state K( tycon_info() );

    ClassInfo class_info;
    class_info.type_vars = class_decl.type_vars;
    class_info.name = class_decl.name;
    class_info.context = class_decl.context;

    // 1. Bind type parameters for class
    K. push_type_var_scope();

    // 1a. Look up kind for this data type.
    auto class_kind = K.kind_for_type_con(class_info.name);

    // 1b. Record the kind for each type variable.
    for(auto& tv: class_decl.type_vars)
        K.bind_type_var(tv, *tv.kind);

    // 2. construct the constraint that represent the class
    Hs::Type class_constraint = Hs::TypeCon(Unlocated(class_decl.name)); // put class_kind into TypeCon?
    for(auto& tv: class_decl.type_vars)
        class_constraint = Hs::TypeApp(class_constraint, tv);

    // 3. make global types for class methods
    global_value_env gve;

    // Add class methods to GVE
    for(auto& sig_decl: class_decl.sig_decls)
    {
        // We need the class variables in scope here.
        auto method_type = check_type(sig_decl.type, K);
        // forall a. C a => method_type
        method_type = Hs::add_constraints({class_constraint}, method_type);
        method_type = Hs::add_forall_vars(class_decl.type_vars, method_type);

        for(auto& v: sig_decl.vars)
        {
            auto& qname = unloc(v.name);
            gve = gve.insert({qname, method_type});
            class_info.members = class_info.members.insert({get_unqualified_name(qname), method_type});
        }
    }

    auto method_matches = get_instance_methods( class_decl.default_method_decls, class_info.members, class_info.name );

    for(auto& [name, match]: method_matches)
    {
        auto dm = get_fresh_Var("dm"+name, true);
        Hs::FunDecl FD(dm, match);
        class_info.default_methods.insert({name, dm});

        auto type = class_info.members.at(name);

        gve = gve.insert({unloc(dm.name), type});
    }


    K.pop_type_var_scope();

    // OK, so now we need to
    //   (a) determine names for dictionary extractors.
    //   (b) determine an order for all the fields.
    //   (c) synthesize field accessors and put them in decls

    // 4. Make global types for superclass extractors
    for(auto& superclass_constraint: class_info.context.constraints)
    {
        string cname1 = get_class_name_from_constraint(superclass_constraint);
        string cname2 = get_class_name_from_constraint(class_constraint);
        string extractor_name = cname1+"From"+cname2;

        auto get_dict = get_fresh_var(extractor_name, true);
        // Should this be a function arrow?
        Hs::Type type = Hs::add_constraints({class_constraint}, superclass_constraint);

        // Maybe intersect the forall_vars with USED vars?
        type = add_forall_vars(class_decl.type_vars, type);
        class_info.superclass_extractors.insert(pair(get_dict, type));

        // Is this right???
        class_info.fields.push_back({get_dict.name, type});
    }
    for(auto& [name,type]: class_info.members)
        class_info.fields.push_back({qualified_name(name), type});

    // 5. Define superclass extractors and member function extractors
    Hs::Decls decls;

    vector<Hs::Type> types;
    for(auto& [name,type]: class_info.fields)
        types.push_back(type);
    Hs::Type dict_type = Hs::tuple_type(types);

    int i = 0;
    int N = class_info.fields.size();
    for(auto& [name,type]: class_info.fields)
    {
        decls.push_back( dictionary_extractor(name, i, N) );

        i++;
    }

    // 6. Load associated type families
    for(auto& type_fam_decl: class_decl.type_fam_decls)
    {
        TypeFamInfo info{type_fam_decl.args, class_decl.type_vars, {}, false};
        type_fam_info().insert({type_fam_decl.con, info});
        if (class_info.associated_type_families.count(type_fam_decl.con))
            throw myexception()<<"Trying to define type family '"<<type_fam_decl.con.print()<<"' twice in class "<<class_decl.name;
        class_info.associated_type_families.insert({type_fam_decl.con,false});
    }

    // 7. Load default associated type family instances
    for(auto& def_inst: class_decl.default_type_inst_decls)
    {
        auto& tf_con = def_inst.con;
        if (not class_info.associated_type_families.count(tf_con))
            throw myexception()<<
                "In class '"<<class_decl.name<<"':\n"<<
                "  In default instance  '"<<def_inst.print()<<"', type family '"<<tf_con<<"' is not defined.";

        // An associated type family can have only one default instance.
        if (class_info.associated_type_families.at(tf_con))
            throw myexception()<<
                "In class '"<<class_decl.name<<"':\n"<<
                "  Associated type family '"<<tf_con.print()<<"' may only have one default instance!";

        // All type arguments must be variables.
        // The type variables may not be repeated.
        set<Hs::TypeVar> lhs_tvs;
        for(auto& arg: def_inst.args)
        {
            auto tv = arg.to<Hs::TypeVar>();
            if (not tv)
                throw myexception()<<
                    "In class '"<<class_decl.name<<"':\n"<<
                    "  In default instance '"<<def_inst.print()<<"' argument '"<<arg.print()<<"' must be a type variable.";
            if (lhs_tvs.count(*tv))
                throw myexception()<<
                    "In class '"<<class_decl.name<<"':\n"<<
                    "  In default instance '"<<def_inst.print()<<"' argument '"<<arg.print()<<"' used twice.";
            lhs_tvs.insert(*tv);
        }

        // The rhs may only mention type vars bound on the lhs.
        for(auto& tv: free_type_variables(def_inst.rhs))
            if (not lhs_tvs.count(tv))
                throw myexception()<<
                    "In class '"<<class_decl.name<<"':\n"<<
                    "  In default instance '"<<def_inst.print()<<"' rhs variable '"<<tv.print()<<"' not bound on the lhs.";

        // This type family has a default now.
        class_info.associated_type_families.at(tf_con) = true;

        TypeFamEqnInfo eqn_info{def_inst.args, def_inst.rhs};

        // Make up an equation id -- this is the "evidence" for the type family instance.
        int eqn_id = FreshVarSource::get_index();
        type_fam_info().at(tf_con).equations.insert({eqn_id, eqn_info});
    }

    return {gve, class_info, decls};
}

Hs::Binds typechecker_state::infer_type_for_classes(const Hs::Decls& decls)
{
    Hs::Binds class_binds;

    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        auto [gve1, class_info, class_decls] = infer_type_for_class(*c);

        gve += gve1;
        class_env().insert({class_info.name, class_info});

        class_binds.push_back(class_decls);
    }

    return class_binds;

    // GVE_C = {method -> type map} :: map<string, polytype> = global_value_env

//     std::cerr<<"GVE (classes):\n";
//    for(auto& [method,type]: gve)
//    {
//        std::cerr<<method<<" :: "<<type.print()<<"\n";
//    }
//    std::cerr<<"\n";

//    std::cerr<<class_binds.print()<<"\n";
//    std::cerr<<"\n";
}

void
typechecker_state::get_type_synonyms(const Hs::Decls& decls)
{
    for(auto& decl: decls)
    {
        auto t = decl.to<Hs::TypeSynonymDecl>();
        if (not t) continue;

        auto name = t->name;
        auto type_vars = t->type_vars;
        auto rhs_type = unloc(t->rhs_type);

        // ensure that these type variables will never occur in the arguments
        substitution_t s;
        for(auto& type_var: type_vars)
        {
            auto new_type_var = fresh_other_type_var();
            s = s.insert({type_var, new_type_var});
            type_var = new_type_var;
        }

        rhs_type = apply_subst(s, rhs_type);

        type_syn_info().insert({name, {name, type_vars, rhs_type}});
    }
}

void typechecker_state::get_type_families(const Hs::Decls& decls)
{
    for(auto& decl: decls)
    {
        if (auto type_fam_decl = decl.to<Hs::TypeFamilyDecl>())
        {
            bool closed = type_fam_decl->where_instances.has_value();
            TypeFamInfo info{type_fam_decl->args, {}, {}, closed};
            type_fam_info().insert({type_fam_decl->con, info});
        }
    }
}
