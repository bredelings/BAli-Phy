/*
  Copyright (C) 2026 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.
*/

#ifndef SEQUENCE_AMBIGUITY_HH
#define SEQUENCE_AMBIGUITY_HH

#include <map>
#include <string>
#include <vector>

#include "sequence/alphabet.hh"

/// Stores the ambiguity state sets used by one encoded character-data object.
class ambiguity_database
{
    struct entry
    {
        alphabet::bitmask_t mask;
        std::vector<double> fmask;
    };

    int n_states_ = 0;
    std::vector<entry> entries_;
    std::map<alphabet::bitmask_t, int> code_for_mask_;

public:
    ambiguity_database() = default;
    explicit ambiguity_database(int n_states);

    int n_states() const {return n_states_;}
    int size() const {return entries_.size();}

    /// Encode a nonempty state set, reusing a code when the set was already observed.
    int encode_mask(const alphabet::bitmask_t& mask);

    /// Return the stored bit mask for an ambiguity code without allocating.
    const alphabet::bitmask_t& mask(int code) const;

    /// Return the stored 0/1 floating-point mask without allocating.
    const std::vector<double>& fmask(int code) const;

    /// Encode one alphabet symbol, storing a proper ambiguity when necessary.
    int encode_symbol(const alphabet& a, const std::string& symbol);

    /// Encode a string into exact, special, and database-local ambiguity codes.
    std::vector<int> encode_sequence(const alphabet& a, const std::string& sequence);

    /// Decode one integer code and report whether its state set was spelled exactly.
    std::pair<std::string, bool> decode(const alphabet& a, int code) const;
};

#endif
