/*
  Copyright (C) 2026 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.
*/

#include "sequence/ambiguity.hh"

#include <boost/dynamic_bitset.hpp>

#include "util/myexception.hh"
#include "util/string/sanitize.hh"

// Bind an initially empty database to one fixed state domain.
ambiguity_database::ambiguity_database(int n_states)
    :n_states_(n_states)
{
    if (n_states < 1)
        throw myexception()<<"An ambiguity database needs at least one state.";
}

// Singleton and complete masks use the existing exact and not-gap encodings;
// only proper, non-singleton subsets consume database entries.
int ambiguity_database::encode_mask(const alphabet::bitmask_t& mask)
{
    if (mask.size() != n_states_)
        throw myexception()<<"Ambiguity mask has "<<mask.size()<<" states, but the alphabet has "<<n_states_<<".";

    auto count = mask.count();
    if (count == 0)
        throw myexception()<<"Cannot encode an empty ambiguity state set.";
    if (count == 1)
        return static_cast<int>(mask.find_first());
    if (count == n_states_)
        return alphabet::not_gap;

    auto found = code_for_mask_.find(mask);
    if (found != code_for_mask_.end())
        return found->second;

    int code = alphabet::first_ambiguity - static_cast<int>(entries_.size());
    std::vector<double> fmask(n_states_, 0.0);
    for (auto state = mask.find_first(); state != alphabet::bitmask_t::npos; state = mask.find_next(state))
        fmask[state] = 1.0;

    entries_.push_back({mask, std::move(fmask)});
    code_for_mask_.emplace(entries_.back().mask, code);
    return code;
}

// Translate the contiguous negative-code range to its vector entry.
const alphabet::bitmask_t& ambiguity_database::mask(int code) const
{
    assert(alphabet::is_ambiguity(code));
    int index = alphabet::first_ambiguity - code;
    assert(index >= 0 and index < entries_.size());
    return entries_[index].mask;
}

// Return the floating mask cached beside the bit mask for legacy consumers.
const std::vector<double>& ambiguity_database::fmask(int code) const
{
    assert(alphabet::is_ambiguity(code));
    int index = alphabet::first_ambiguity - code;
    assert(index >= 0 and index < entries_.size());
    return entries_[index].fmask;
}

// Handle global and exact codes without allocating a mask, then store any
// recognized ambiguity in this data-local database.
std::optional<int> ambiguity_database::encode_symbol(const alphabet& a, const std::string& symbol)
{
    if (a.n_letters() != n_states_)
        throw myexception()<<"Cannot encode alphabet '"<<a.name<<"' with an ambiguity database for "<<n_states_<<" states.";

    if (symbol == a.gap_letter)
        return alphabet::gap;
    if (symbol == a.wildcard)
        return alphabet::not_gap;
    for (const auto& unknown_letter: a.unknown_letters)
        if (symbol == unknown_letter)
            return alphabet::unknown;
    if (auto state = a.find_letter(symbol))
        return *state;
    if (auto mask = a.mask_for_symbol(symbol))
        return encode_mask(*mask);
    return {};
}

// Split according to the alphabet width so product-alphabet symbols such as RAY
// are encoded as one state set rather than collapsed position by position.
std::vector<int> ambiguity_database::encode_sequence(const alphabet& a, const std::string& sequence)
{
    int symbol_width = a.width();
    if (sequence.size() % symbol_width != 0)
    {
        a.diagnose_sequence_encoding_failure(sequence);
        throw myexception()<<"Alignment row has "<<sequence.size()<<" columns, but alphabet '"<<a.name
                           <<"' requires a multiple of "<<symbol_width<<" columns.";
    }

    std::vector<int> result(sequence.size() / symbol_width);
    for (int i = 0; i < result.size(); i++)
    {
        std::string symbol = sequence.substr(i * symbol_width, symbol_width);
        auto code = encode_symbol(a, symbol);
        if (not code)
        {
            a.diagnose_sequence_encoding_failure(sequence);
            throw myexception()<<"Unrecognized symbol '"<<sanitize_string(symbol)<<"' for alphabet '"<<a.name
                               <<"' at character "<<i + 1<<".";
        }
        result[i] = *code;
    }
    return result;
}

// Exact and special codes retain alphabet spelling; local ambiguities use their stored mask.
std::pair<std::string, bool> ambiguity_database::decode(const alphabet& a, int code) const
{
    if (alphabet::is_ambiguity(code))
        return a.lookup(mask(code));

    return {a.lookup(code), true};
}
