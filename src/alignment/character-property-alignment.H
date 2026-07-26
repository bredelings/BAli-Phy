#ifndef CHARACTER_PROPERTY_ALIGNMENT_H
#define CHARACTER_PROPERTY_ALIGNMENT_H

#include <filesystem>
#include <istream>
#include <string>
#include <vector>

#include "character-properties.H"
#include "sequence/alphabet.H"
#include "sequence/sequence.H"

namespace character_properties
{

struct alignment_token
{
    int alphabet_code;
    int character_index;
};

struct tokenized_alignment
{
    int token_width;
    std::vector<std::vector<alignment_token>> rows;

    const std::vector<alignment_token>& operator[](std::size_t index) const {return rows[index];}
    std::size_t n_columns() const {return rows.empty() ? 0 : rows.front().size();}
};

/// Load and pad a template alignment without removing its all-gap columns.
std::vector<sequence> load_template_alignment(std::istream& input);

/// Load and pad a named template alignment without removing its all-gap columns.
std::vector<sequence> load_template_alignment(const std::filesystem::path& filename);

/// Split alignment rows into alphabet-width cells and assign ungapped character coordinates.
tokenized_alignment tokenize_alignment(const std::vector<sequence>& sequences, const alphabet* alph);

/// Require each displayed sequence to have one complete summary value per non-gap character.
void validate_for_alignment(const summary& properties, const std::vector<sequence>& sequences,
                            const tokenized_alignment& tokens);

}

#endif
