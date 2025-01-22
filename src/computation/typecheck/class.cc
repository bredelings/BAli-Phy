#include "typecheck.H"
#include "kindcheck.H"
#include "haskell/ids.H"
#include "computation/core/func.H"

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
        Core2::Pattern<> pattern = Core2::ConPat<>{con_name, fields};

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
    push_note( Note()<<"In class '"<<class_decl.con<<"':" );

    ClassInfo class_info;
    string class_name = unloc(class_decl.con).name;
    class_info.type_vars = desugar(class_decl.type_vars);
    class_info.name = unloc(class_decl.con).name;
    class_info.context = desugar(class_decl.context);

    // 2. construct the constraint that represent the class
    auto hs_class_constraint = Hs::type_apply(class_decl.con, class_decl.type_vars);
    auto class_constraint = desugar(hs_class_constraint);

    // 3. make global types for class methods

    // Add class methods to GVE
    for(auto& sig_decl: class_decl.sig_decls)
    {
        // first: quantify by variables NOT in the class decl,
        // then : add the class constraint
        // then : quantify by the variables in the class decl.

        // forall a. C a => method_type
        auto hs_method_type = sig_decl.type;
        hs_method_type = Hs::add_constraints({hs_class_constraint}, hs_method_type);
        hs_method_type = Hs::add_forall_vars(class_decl.type_vars, hs_method_type);
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
        auto dm = get_fresh_Var("dm"+method_name, true);
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
        // Should this be a function arrow?
        Type type = add_constraints({class_constraint}, superclass_constraint);

        // Maybe intersect the forall_vars with USED vars?
        type = add_forall_vars( desugar(class_decl.type_vars), type);
        class_info.superclass_extractors.insert(pair(make_core_var(get_dict), type));

        // Is this right???
        class_info.fields.push_back(pair(get_dict, type));
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
        decls.push_back( {Core2::Var<>(var.name), make_field_extractor(class_info.name, i, N, *this)} );

        i++;
    }

    // 6. Load associated type families
    for(auto& type_fam_decl: class_decl.fam_decls)
    {
        auto args = desugar(type_fam_decl.args);
        auto fname = unloc(type_fam_decl.con).name;
        auto kind = this_mod().lookup_local_type(fname)->kind;

        auto con = desugar(type_fam_decl.con);
        this_mod().lookup_local_type(con.name)->is_type_fam()->info = std::make_shared<TypeFamInfo>(args, kind, class_name);
        if (class_info.associated_type_families.count(con))
            throw note_exception()<<"Trying to define type family '"<<con.print()<<"' twice";
        class_info.associated_type_families.insert({con,{}});
    }

    // 7. Load default associated type family instances
    for(auto& def_inst: class_decl.default_type_inst_decls)
    {
        push_note( Note()<<"In default instance '"<<def_inst.print()<<"':");

        auto tf_con = desugar(def_inst.con);
        if (not class_info.associated_type_families.count(tf_con))
            throw note_exception()<<
                "  Type family '"<<tf_con<<"' is not defined.";

        // An associated type family can have only one default instance.
        if (class_info.associated_type_families.at(tf_con))
            throw note_exception()<<
                "  Associated type family '"<<tf_con.print()<<"' may only have one default instance!";

        // All type arguments must be variables.
        // The type variables may not be repeated.
        set<TypeVar> lhs_tvs;
        for(auto& arg: desugar(def_inst.args))
        {
            auto tv = arg.to<TypeVar>();

            if (not tv)
                throw note_exception()<<
                    "  Argument '"<<arg.print()<<"' must be a type variable.";

            if (lhs_tvs.count(*tv))
                throw note_exception()<<
                    "  Argument '"<<arg.print()<<"' used twice.";

            lhs_tvs.insert(*tv);
        }

        // The rhs may only mention type vars bound on the lhs.
        for(auto& tv: free_type_variables(desugar(def_inst.rhs)))
            if (not lhs_tvs.count(tv))
                throw note_exception()<<"  rhs variable '"<<tv.print()<<"' not bound on the lhs.";

        // This type family has a default instance now.
        class_info.associated_type_families.at(tf_con) = desugar(def_inst);

        // Add the default type instance -- no need for variables to match the class.
        // check_add_type_instance(def_inst, unloc(class_decl.name), {});

        pop_note();
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

        auto name = unloc(t->con).name;
        auto type_vars = desugar(t->type_vars);
        auto rhs_type = desugar(t->rhs_type);

        // ensure that these type variables will never occur in the arguments
        substitution_t s;
        for(auto& type_var: type_vars)
        {
            auto new_type_var = fresh_other_type_var();
            s = s.insert({type_var, new_type_var});
            type_var = new_type_var;
        }

        rhs_type = apply_subst(s, rhs_type);

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
            auto args = desugar(type_fam_decl->args);
            auto fname = unloc(type_fam_decl->con).name;
            auto kind = this_mod().lookup_local_type(fname)->kind;

            auto con = desugar(type_fam_decl->con);
            this_mod().lookup_local_type(con.name)->is_type_fam()->info = std::make_shared<TypeFamInfo>(args,kind);

            // Add instance equations for closed type families
            if (type_fam_decl->where_instances)
            {
                for(auto& inst: *type_fam_decl->where_instances)
                    check_add_type_instance(inst, {}, {});
                this_mod().lookup_local_type(con.name)->is_type_fam()->info->closed = true;
            }
        }
    }
}
