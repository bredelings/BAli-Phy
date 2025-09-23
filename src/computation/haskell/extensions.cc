#include "extensions.H"
#include "util/myexception.H"

using std::set;
using std::map;
using std::string;
using std::optional;

map<string,LangExt> ext_to_bit =
{
    {"DataKinds",                   {LangExt::DataKinds}},
    {"EmptyDataDecls",              {LangExt::EmptyDataDecls}},
    {"ExistentialQuantification",   {LangExt::ExistentialQuantification}},
    {"ExtendedDefaultRules",        {LangExt::ExtendedDefaultRules}},
    {"ExplicitForall",              {LangExt::ExplicitForall}},
    {"FieldSelectors",              {LangExt::FieldSelectors}},
    {"FlexibleInstances",           {LangExt::FlexibleInstances}},
    {"FunctionalDependencies",      {LangExt::FunctionalDependencies}},  // implies MultiParamTypeClasses
    {"ForeignFunctionInterface",    {LangExt::ForeignFunctionInterface}},
    {"GADTSyntax",                  {LangExt::GADTSyntax}},
    {"GADTs",                       {LangExt::GADTs}},                // implies MonoLocalBinds
    {"ImplicitPrelude",             {LangExt::ImplicitPrelude}},
    {"ImpredicativeTypes",          {LangExt::ImpredicativeTypes}},   // implies RankNTypes
    {"Incoherent Instances",        {LangExt::IncoherentInstances}},  // DEPRECATED
    {"KindSignatures",              {LangExt::KindSignatures}},
    {"LexicalNegation",             {LangExt::LexicalNegation}},
    {"MonoLocalBinds",              {LangExt::MonoLocalBinds}},
    {"MultiParamTypeClasses",       {LangExt::MultiParamTypeClasses}},
    {"NamedDefaults",               {LangExt::NamedDefaults}},
    {"OverloadedRecordDot",         {LangExt::OverloadedRecordDot}},
    {"OverloadedLists",             {LangExt::OverloadedLists}},
    {"OverloadedStrings",           {LangExt::OverloadedStrings}},
    {"Overlapping Instances",       {LangExt::OverlappingInstances}},  // DEPRECATED
    {"PolyKinds",                   {LangExt::PolyKinds}},
    {"RankNTypes",                  {LangExt::RankNTypes}},            // implies ExplicitForall
    {"RecursiveDo",                 {LangExt::RecursiveDo}},
    {"TypeFamilies",                {LangExt::TypeFamilies}},          // implies MonoLocalBinds
    {"TypeOperators",               {LangExt::TypeOperators}},
    {"ScopedTypeVariables",         {LangExt::ScopedTypeVariables}},
    {"StarIsType",                  {LangExt::StarIsType}},
};

set<LangExt> haskell98_extensions =
{
    LangExt::ImplicitPrelude,
    LangExt::StarIsType,
//    LangExt::CUSKs,
//    LangExt::MonomorphismRestriction,
//    LangExt::NPlusKPatterns,
//    LangExt::DatatypeContexts,
//    LangExt::TraditionalRecordSyntax,
    LangExt::FieldSelectors,
//    LangExt::NondecreasingIndentation
//    LangExt::DeepSubsumption
};

set<LangExt> haskell2010_extensions =
{
    LangExt::ImplicitPrelude,
    LangExt::StarIsType,
//    LangExt::CUSKs,
//    LangExt::MonomorphismRestriction,
//    LangExt::NPlusKPatterns,
//    LangExt::DatatypeContexts,
//    LangExt::TraditionalRecordSyntax,
//    LangExt::EmptyDataDecls,
//    LangExt::ForeignFunctionInterface,
    LangExt::FieldSelectors,
//    LangExt::PatternGuards,
//    LangExt::DoAndIfThenElse,
//    LangExt::RelaxedPolyRec,
//    LangExt::DeepSubsumption
};

set<LangExt> ghc2021_extensions =
{
    LangExt::ImplicitPrelude,
//    LangExt::StarIsType,
//    LangExt::MonomorphismRestriction,
//    LangExt::TraditionalRecordSyntax,
//    LangExt::EmptyDataDecls,
//    LangExt::ForeignFunctionInterface,
//    LangExt::PatternGuards,
//    LangExt::DoAndIfThenElse,
    LangExt::FieldSelectors,
//    LangExt::RelaxedPolyRec,

//    LangExt::BangPatterns,
//    LangExt::BinaryLiterals,
//    LangExt::ConstrainedClassMethods,
//    LangExt::ConstraintKinds,
//    LangExt::DeriveDataTypeable,
//    LangExt::DeriveFoldable,
//    LangExt::DeriveFunctor,
//    LangExt::DeriveGeneric,
//    LangExt::DeriveLift,
//    LangExt::DeriveTraversable,
//    LangExt::EmptyCase,
//    LangExt::EmptyDataDeriving,
//    LangExt::ExistentialQuantification,
//    LangExt::ExplicitForAll,
//    LangExt::FlexibleContexts,
//    LangExt::FlexibleInstances,
//    LangExt::GADTSyntax,
//    LangExt::GeneralizedNewtypeDeriving,
//    LangExt::HexFloatLiterals,
//    LangExt::ImportQualifiedPost,
//    LangExt::InstanceSigs,
//    LangExt::KindSignatures,
//    LangExt::MultiParamTypeClasses,
//    LangExt::NamedFieldPuns,
//    LangExt::NamedWildCards,
//    LangExt::NumericUnderscores,
//    LangExt::PolyKinds,
//    LangExt::PostfixOperators,
//    LangExt::RankNTypes,
//    LangExt::ScopedTypeVariables,
//    LangExt::TypeAbstractions,
//    LangExt::StandaloneDeriving,
//    LangExt::StandaloneKindSignatures,
//    LangExt::TupleSections,
//    LangExt::TypeApplications,
//    LangExt::TypeOperators,
//    LangExt::TypeSynonymInstances
};

set<LangExt> default_extensions =
{
    LangExt::ImplicitPrelude,
    LangExt::FieldSelectors
};

bool LanguageExtensions::has_extension(LangExt opt) const
{
    return extension_bits[int(opt)];
}


void LanguageExtensions::set_extension(LangExt opt, bool value)
{
    extension_bits[int(opt)] = value;
}

optional<Note> LanguageExtensions::set_option(const string& opt)
{
    // 1. Get positive or negative sense
    auto ext = opt;
    bool value = true;
    if (ext.starts_with("No"))
    {
        ext = ext.substr(2);
        value = false;
    }

    // 2. Check if the option is recognized
    if (not ext_to_bit.count(ext))
        return Note()<<"Option "<<opt<<" unrecognized";

    // 3. Check if we've already set this
    if (already_set.count(ext))
    {
        auto prev_opt = already_set.at(ext);
        if (opt == prev_opt)
            return Note()<<"Option "<<opt<<" set twice";
        else
            return Note()<<"Option "<<opt<<" conflicts with previous option '"<<prev_opt<<"'";
    }
    else
        already_set.insert({ext, opt});

    // 3. Set the bit
    auto extension = ext_to_bit.at(ext);
    set_extension( extension, value );

    return {};
}

LanguageExtensions::LanguageExtensions()
    :extension_bits(int(LangExt::Last))
{
    for(auto& ext: default_extensions)
        set_extension(ext);
}
