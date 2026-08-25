#include "character-property-alignment.hh"

#include <map>
#include <optional>

#include "sequence/codons.hh"
#include "sequence/sequence-format.hh"
#include "util/io.hh"
#include "util/myexception.hh"

namespace character_properties
{

/// Load and pad a template alignment without removing its all-gap columns.
std::vector<sequence> load_template_alignment(std::istream& input)
{
    auto sequences = sequence_format::read_guess(input);
    if (sequences.empty())
        throw myexception()<<"Alignment did not contain any sequences.";
    pad_to_same_length(sequences);
    return sequences;
}

/// Load and pad a named template alignment without removing its all-gap columns.
std::vector<sequence> load_template_alignment(const std::filesystem::path& filename)
{
    checked_ifstream input(filename, "alignment file");
    try
    {
        return load_template_alignment(input);
    }
    catch (myexception& error)
    {
        error.prepend("File '"+filename.string()+"': ");
        throw;
    }
}

/// Split alignment rows into alphabet-width cells and assign ungapped character coordinates.
tokenized_alignment tokenize_alignment(const std::vector<sequence>& sequences, const alphabet* alph)
{
    int token_width = alph ? alph->width() : 1;
    ambiguity_database ambiguities = alph ? ambiguity_database(alph->n_letters()) : ambiguity_database();
    std::vector<std::vector<alignment_token>> rows;
    std::optional<std::size_t> n_columns;

    for (const auto& sequence: sequences)
    {
        std::vector<int> codes;
        if (alph)
            codes = ambiguities.encode_sequence(*alph, static_cast<const std::string&>(sequence));
        else
        {
            codes.reserve(sequence.size());
            for (char letter: sequence)
            {
                if (letter == '-')
                    codes.push_back(alphabet::gap);
                else if (letter == '?' or letter == '=')
                    codes.push_back(alphabet::unknown);
                else
                    codes.push_back(0);
            }
        }

        if (not n_columns)
            n_columns = codes.size();
        else if (codes.size() != *n_columns)
            throw myexception()<<"Sequence '"<<sequence.name<<"' has "<<codes.size()
                               <<" alignment cells, but the first sequence has "<<*n_columns<<".";

        std::vector<alignment_token> tokens;
        tokens.reserve(codes.size());
        int character_index = 0;
        for (int code: codes)
        {
            int index = -1;
            if (alphabet::is_character(code))
                index = character_index++;
            tokens.push_back({code, index});
        }
        rows.push_back(std::move(tokens));
    }

    if (not n_columns or *n_columns == 0)
        throw myexception()<<"Alignment did not contain any character columns.";
    return {token_width, std::move(ambiguities), std::move(rows)};
}

/// Require each displayed sequence to have one complete summary value per non-gap character.
void validate_for_alignment(const summary& properties, const std::vector<sequence>& sequences,
                            const tokenized_alignment& tokens)
{
    std::map<std::string, std::size_t> character_counts;
    for (std::size_t sequence_index = 0; sequence_index < sequences.size(); sequence_index++)
    {
        std::size_t count = 0;
        for (const auto& token: tokens[sequence_index])
            if (token.character_index >= 0)
                count++;
        if (not character_counts.emplace(sequences[sequence_index].name, count).second)
            throw myexception()<<"Alignment contains duplicate sequence name '"
                               <<sequences[sequence_index].name<<"'.";
    }

    for (const auto& [property_name, property]: properties.properties)
    {
        for (const auto& [sequence_name, expected_count]: character_counts)
        {
            auto found = property.mean.find(sequence_name);
            if (found == property.mean.end())
                throw myexception()<<"Property '"<<property_name
                                   <<"': mean values are missing sequence '"<<sequence_name<<"'.";
            if (found->second.size() != expected_count)
                throw myexception()<<"Property '"<<property_name<<"': sequence '"<<sequence_name<<"' has "
                                   <<found->second.size()<<" values, but the alignment has "<<expected_count
                                   <<" characters.";
        }
    }
}

/// Map template columns to their observed sequence characters and optional codon translations.
alignment_projection project_alignment(const std::vector<sequence>& sequences, const tokenized_alignment& tokens,
                                       const alphabet& alph)
{
    alignment_projection result(tokens.n_columns());
    const auto* codons = dynamic_cast<const Codons*>(&alph);

    for (std::size_t column = 0; column < tokens.n_columns(); column++)
    {
        auto& projected_column = result[column];
        projected_column.alignment_column = column;
        for (std::size_t sequence_index = 0; sequence_index < sequences.size(); sequence_index++)
        {
            const auto& token = tokens[sequence_index][column];
            if (token.character_index < 0)
                continue;

            std::optional<std::string> translation;
            // An ambiguous codon has a definite translation only when every
            // represented codon maps to the same amino acid.
            if (codons)
            {
                int amino_acid = alphabet::not_gap;
                if (token.alphabet_code >= 0)
                    amino_acid = codons->translate(token.alphabet_code);
                else if (alphabet::is_ambiguity(token.alphabet_code))
                {
                    amino_acid = -1;
                    const auto& mask = tokens.ambiguities.mask(token.alphabet_code);
                    for (auto state = mask.find_first(); state != alphabet::bitmask_t::npos; state = mask.find_next(state))
                    {
                        int translated = codons->translate(state);
                        if (amino_acid != -1 and amino_acid != translated)
                        {
                            amino_acid = alphabet::not_gap;
                            break;
                        }
                        amino_acid = translated;
                    }
                }
                translation = codons->getAminoAcids().lookup(amino_acid);
            }
            projected_column.characters.push_back({
                sequence_index,
                sequences[sequence_index].name,
                static_cast<std::size_t>(token.character_index),
                token.alphabet_code,
                sequences[sequence_index].substr(column * tokens.token_width, tokens.token_width),
                std::move(translation)
            });
        }
    }
    return result;
}

}
