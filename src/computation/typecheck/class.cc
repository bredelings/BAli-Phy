#include "typecheck.H"
#include "kindcheck.H"
#include "haskell/ids.H"
#include "computation/core/func.H"
#include "computation/rename/rename.H"
#include "tidy.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

Core2::Exp<> make_field_extractor(const string& con_name, int i, int N, FreshVarSource& source)
{
    Core2::Var<> x("x");

    if (N == 1)
        return  lambda_quantify({x}, Core2::Exp<>(x));
    else
    {
        // extractor (_,field,_,_) = field;
        // extractor = \x -> case x of ConName _ d _ _ -> d

        Core2::Var<> d("d");
    
        vector<Core2::Var<>> fields;
        for(int j=0;j<N;j++)
        {
            if (i == j)
                fields.push_back( d );
            else
                fields.push_back( source.get_fresh_core_var("w") );
        }
        Core2::Pattern<> pattern{con_name, fields};

        return lambda_quantify({x}, Core2::Exp<>(Core2::Case<>{x,{{pattern, d}}}));
    }
}


Hs::Var unqualified(Hs::Var v)
{
    v.name = get_unqualified_name(v.name);
    return v;
}

// OK, so
// * global_value_env    = name         :: forall a: class var => signature (i.e. a-> b -> a)
// * global_instance_env = made-up-name :: forall a: class var => superclass var
// * Hs::Decls           = { name         = \dict -> case dict of K _ _ method _ _ -> method }
//                       = { made-up-name = \dict -> case dict of K superdict _ _ _ _ -> superdict }

tuple<ClassInfo, Core2::Decls<>>
TypeChecker::infer_type_for_class(const Hs::ClassDecl& class_decl)
{
    push_note( Note()<<"In class '"<<TidyState().print(TypeCon(unloc(class_decl.con).name))<<"':" );

    // 1. construct the constraint that represent the class
    auto hs_class_constraint = Hs::type_apply(class_decl.con, class_decl.type_vars);
    auto hs_class_type = Hs::quantify(class_decl.type_vars, class_decl.context, hs_class_constraint);

    auto class_type = check_constraint(hs_class_type);

    auto [tvs, context, class_constraint] = peel_top_gen(class_type);

    string class_name = unloc(class_decl.con).name;

    ClassInfo class_info;
    class_info.type_vars = tvs;
    class_info.name = class_name;
    class_info.context = context;

    // 3. make global types for class methods

    // Add class methods to GVE
    for(auto& sig_decl: class_decl.sig_decls)
    {
        // forall a. C a => method_type  (non-class variables are quantified in rename)
        auto hs_method_type = Hs::quantify(class_decl.type_vars, {hs_class_constraint}, sig_decl.type);
        auto method_type = check_type(hs_method_type);

        for(auto& lv: sig_decl.vars)
        {
            auto& v = unloc(lv);
            auto S = this_mod().lookup_local_symbol(v.name);
            S->type = method_type;
            class_info.members = class_info.members.insert({unqualified(v), method_type});
        }
    }

    auto method_matches = get_instance_methods( class_decl.default_method_decls, class_info.members, class_info.name );

    for(auto& [method, match]: method_matches)
    {
        auto& method_name = method.name;
        auto dm = get_fresh_Var("dm$"+method_name, true);
        Hs::FunDecl FD({noloc,dm}, match);
        class_info.default_methods.insert({method_name, dm});

        auto type = class_info.members.at(method_name);

        // Hmm... so one issue is that I think we want to declare this (i) with NO alias and (ii) only with the original name.
        auto S = symbol_info(dm.name, symbol_type_t::default_method, class_name, {}, {});
        S.type = type;

        // Default methods don't get an alias.
        this_mod().add_symbol(S);
    }

    // OK, so now we need to
    //   (a) determine names for dictionary extractors.
    //   (b) determine an order for all the fields.
    //   (c) synthesize field accessors and put them in decls

    // 4. Make global types for superclass extractors
    for(auto& superclass_constraint: class_info.context)
    {
        // We don't reserve a field in the dictionary for equality superclasses.
        if (is_equality_pred(superclass_constraint)) continue;

        string cname1 = get_class_name_from_constraint(superclass_constraint);
        string cname2 = get_class_name_from_constraint(class_constraint);
        string extractor_name = cname1+"From"+cname2;

        auto get_dict = get_fresh_Var(extractor_name, true);
        // Maybe intersect the forall_vars with USED vars?
        // Should the => be a function arrow?
        auto dict_type = quantify(tvs, {class_constraint}, superclass_constraint);
        class_info.superclass_extractors.insert(pair(make_core_var(get_dict), dict_type));

        // Is this right???
        class_info.fields.push_back(pair(get_dict, dict_type));

        // Create the symbol for the superclass selector
        auto S = symbol_info(get_dict.name, symbol_type_t::superclass_selector, class_name, {}, {});
        S.type = dict_type;

        // Default methods don't get an alias.
        this_mod().add_symbol(S);
    }
    for(auto& [var,type]: class_info.members)
        class_info.fields.push_back({add_mod_name(var), type});

    // 5. Define superclass extractors and member function extractors
    Core2::Decls<> decls;

    vector<Type> types;
    for(auto& [_,type]: class_info.fields)
        types.push_back(type);
    Type dict_type = tuple_type(types);

    int i = 0;
    int N = class_info.fields.size();
    for(auto& [var, type]: class_info.fields)
    {
        auto S = this_mod().lookup_local_symbol(var.name);
        S->unfolding = MethodUnfolding{i};

        decls.push_back( {Core2::Var<>(var.name), make_field_extractor(class_info.name, i, N, *this)} );

        i++;
    }

    // 6. Load associated type families
    for(auto& type_fam_decl: class_decl.fam_decls)
    {
        auto hs_type_fam_type = Hs::quantify(type_fam_decl.args, {}, Hs::type_apply(type_fam_decl.con, type_fam_decl.args));
        auto type_fam_type = check_type(hs_type_fam_type);
        auto [args, _, head] = peel_top_gen(type_fam_type);
        
        auto fname = unloc(type_fam_decl.con).name;
        auto kind = this_mod().lookup_local_type(fname)->kind;

        TypeCon con(unloc(type_fam_decl.con).name);
        this_mod().lookup_local_type(con.name)->is_type_fam()->info = std::make_shared<TypeFamInfo>(args, kind, class_name);
        if (class_info.associated_type_families.count(con))
        {
            record_error(hs_type_fam_type.loc, Note()<<"Trying to define type family '"<<con.print()<<"' twice");
            continue;
        }
        class_info.associated_type_families.insert({con,{}});
    }

    // 7. Load default associated type family instances
    for(auto& def_inst: class_decl.default_type_inst_decls)
    {
        int nerrors = num_errors();

        //push_note( Note()<<"In default instance '"<<def_inst.print()<<"':");

        TypeCon tf_con(unloc(def_inst.con).name);
        if (not class_info.associated_type_families.count(tf_con))
            record_error(def_inst.con.loc, Note()<<"Type family '"<<tf_con<<"' is not defined in class '"<<class_name<<"'");

        // An associated type family can have only one default instance.
        if (class_info.associated_type_families.at(tf_con))
            record_error(def_inst.con.loc, Note()<<"Associated type family '"<<tf_con.print()<<"' may only have one default instance!");

        // All type arguments must be variables.
        // The type variables may not be repeated.
        set<Hs::TypeVar> lhs_tvs;
        for(auto& [loc,arg]: def_inst.args)
        {
            auto tv = arg.to<Hs::TypeVar>();

            if (not tv)
                record_error(loc, Note()<<"Argument '"<<arg.print()<<"' must be a type variable.");
            else if (lhs_tvs.count(*tv))
                record_error(loc, Note()<<"Argument '"<<arg.print()<<"' used twice in default instance.");
            else
                lhs_tvs.insert(*tv);
        }

        if (num_errors() > nerrors) continue;

        auto hs_lhs = Hs::type_apply(def_inst.con, def_inst.args);
        auto [free_tvs, lhs, rhs] = check_type_instance(hs_lhs, def_inst.rhs);
        auto [inst_con, inst_args] = decompose_type_apps(lhs);

        // This type family has a default instance now.
        class_info.associated_type_families.at(tf_con) = TypeFamilyInstanceDecl{tf_con, inst_args, rhs};

        // Add the default type instance -- no need for variables to match the class.
        // check_add_type_instance(def_inst, unloc(class_decl.name), {});

        // pop_note();
    }

    pop_note();
    return {class_info, decls};
}

Core2::Decls<> TypeChecker::infer_type_for_classes(const Hs::Decls& hs_decls)
{
    Core2::Decls<> core_decls;

    for(auto& [_,decl]: hs_decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        auto [class_info, class_decls] = infer_type_for_class(*c);

        this_mod().lookup_local_type(class_info.name)->is_class()->info = std::make_shared<ClassInfo>(class_info);

        for(auto& core_decl: class_decls)
            core_decls.push_back(core_decl);
    }

    return core_decls;

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
TypeChecker::get_type_synonyms(const Hs::Decls& decls)
{
    for(auto& [_,decl]: decls)
    {
        auto t = decl.to<Hs::TypeSynonymDecl>();
        if (not t) continue;

        auto hs_lhs = Hs::type_apply(t->con, t->type_vars);
        auto hs_syn_type = Hs::quantify(t->type_vars,{}, Hs::make_equality_type(hs_lhs, t->rhs_type));

        auto syn_type = check_constraint(hs_syn_type);

        auto [type_vars, context, equality] = peel_top_gen(syn_type);
        auto [core_sim, args] = decompose_type_apps(equality);
        auto lhs_type = args[0];
        auto rhs_type = args[1];

        auto name = unloc(t->con).name;

        auto& info = this_mod().lookup_local_type(name)->is_type_syn()->info;
        assert(not info);
        info = std::shared_ptr<TypeSynonymInfo>(new TypeSynonymInfo{name, type_vars, rhs_type});
    }
}

void TypeChecker::get_type_families(const Hs::Decls& decls)
{
    for(auto& [_,decl]: decls)
    {
        if (auto type_fam_decl = decl.to<Hs::FamilyDecl>())
        {
            auto fam_name = unloc(type_fam_decl->con).name;
            auto fam_kind = this_mod().lookup_local_type(fam_name)->kind;

            auto kind = fam_kind;
            auto& hs_args = type_fam_decl->args;
            vector<TypeVar> args;
            for(auto& [loc,tv]: hs_args)
            {
                auto [first,rest] = *is_gen_function_type(kind);
                args.push_back(TypeVar(tv.name, first));
                kind = rest;
            }

            this_mod().lookup_local_type(fam_name)->is_type_fam()->info = std::make_shared<TypeFamInfo>(args, fam_kind);

            // Add instance equations for closed type families
            if (type_fam_decl->where_instances)
            {
                for(auto& inst: *type_fam_decl->where_instances)
                    check_add_type_instance(inst, {}, {});
                this_mod().lookup_local_type(fam_name)->is_type_fam()->info->closed = true;
            }
        }
    }
}
