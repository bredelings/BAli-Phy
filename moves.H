#ifndef MOVES_H
#define  MOVES_H
#include "alignment.H"
#include "tree.H"

alignment swap_gap(const alignment&);
alignment inc_length(const alignment&);
alignment dec_length(const alignment&);

alignment move(const alignment&,const tree&);
#endif
