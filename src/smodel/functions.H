#ifndef SMODEL_FUNCTIONS_H
#define SMODEL_FUNCTIONS_H

#include "computation/prelude.H"

extern const expression_ref MultiParameter;
extern const expression_ref rate;
extern const expression_ref scale;
extern const expression_ref QExp;
extern const expression_ref ReversibleMarkov;
extern const expression_ref ReversibleFrequency;
extern const expression_ref Q_from_S_and_R;

extern const expression_ref MixtureModel;
extern const expression_ref n_base_models;
extern const expression_ref state_letters;
extern const expression_ref n_states;
extern const expression_ref get_alphabet;
extern const expression_ref get_frequencies;
extern const expression_ref get_component_frequencies;
extern const expression_ref base_model;
extern const expression_ref distribution;
extern const expression_ref MultiRate;

Program SModel_Functions();

#endif
