#ifndef HASKELL_OPTIONS_H
#define HASKELL_OPTIONS_H

#include <map>
#include <set>
#include <string>
#include <optional>
#include <boost/dynamic_bitset.hpp>
#include "computation/message.H"

enum class LangExt
{
    First=0,
    ImplicitPrelude,
    StarIsType,
    CUSKs,
    MonomorphismRestriction,
    NPlusKPatterns,
    DatatypeContexts,
    TraditionalRecordSyntax,
    FieldSelectors,
    // NondecreasingIndentation
    // DeepSubsumption
    EmptyDataDecls,
    ForeignFunctionInterface,
    PatternGuards,
    DoAndIfThenElse,
    RelaxedPolyRec,
    BangPatterns,
    BinaryLiterals,
    ConstrainedClassMethods,
    ConstraintKinds,
    DeriveDataTypeable,
    DeriveFoldable,
    DeriveFunctor,
    DeriveGeneric,
    DeriveLift,
    DeriveTraversable,
    Emptycase,
    EmptyDataDeriving,
    ExistentialQuantification,
    ExplicitForall,
    FlexibleContexts,
    FlexibleInstances,
    GADTSyntax,
    GeneralizedNewtypeDeriving,
    HexFloatLiterals,
    ImportQualifiedPost,
    InstanceSigs,
    KindSignatures,
    LexicalNegation,
    MultiParamTypeClasses,
    NamedFieldPuns,
    NamedWildCards,
    NumericUnderscores,
    OverloadedStrings,
    OverloadedRecordDot,
    PolyKinds,
    Postfixoperators,
    RankNTypes,
    ScopedTypeVariables,
    TypeAbstractions,
    StandaloneDeriving,
    StandaloneKindSignatures,
    TypeSections,
    TypeApplications,
    TypeOperators,
    TypeSynonymInstances,
    Last
};

struct LanguageExtensions
{
    std::map<std::string, std::string> already_set;

    boost::dynamic_bitset<> extension_bits;

    bool has_extension(LangExt) const;
    void set_extension(LangExt, bool value=true);

    std::optional<Note> set_option(const std::string& opt);

    LanguageExtensions();
};

#endif
