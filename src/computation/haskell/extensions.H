#ifndef HASKELL_OPTIONS_H
#define HASKELL_OPTIONS_H

#include <map>
#include <set>
#include <string>
#include <optional>
#include <boost/dynamic_bitset.hpp>
#include "util/assert.hh"   // Avoid dynamic_bitset.hpp overwriting our assert definition.
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
    DataKinds,
    DeriveDataTypeable,
    DeriveFoldable,
    DeriveFunctor,
    DeriveGeneric,
    DeriveLift,
    DeriveTraversable,
    EmptyCase,
    EmptyDataDeriving,
    ExistentialQuantification,
    ExtendedDefaultRules,
    ExplicitForall,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    GADTSyntax,
    GADTs,
    GeneralizedNewtypeDeriving,
    HexFloatLiterals,
    ImportQualifiedPost,
    ImpredicativeTypes,
    IncoherentInstances,
    InstanceSigs,
    KindSignatures,
    LexicalNegation,
    MonoLocalBinds,
    MultiParamTypeClasses,
    NamedDefaults,
    NamedFieldPuns,
    NamedWildCards,
    NumericUnderscores,
    OverloadedLists,
    OverloadedStrings,
    OverloadedRecordDot,
    OverlappingInstances,
    PolyKinds,
    Postfixoperators,
    RankNTypes,
    RecursiveDo,
    ScopedTypeVariables,
    StandaloneDeriving,
    StandaloneKindSignatures,
    TypeAbstractions,
    TypeApplications,
    TypeFamilies,
    TypeOperators,
    TypeSections,
    TypeSynonymInstances,
    Last
};

struct LanguageExtensions
{
    std::map<std::string, std::string> already_set;

    std::vector<bool> extension_bits;

    bool has_extension(LangExt) const;
    void set_extension(LangExt, bool value=true);

    std::optional<Note> set_option(const std::string& opt);

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(already_set, extension_bits);
    }

    LanguageExtensions();
};

#endif
