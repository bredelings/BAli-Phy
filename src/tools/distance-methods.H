#ifndef DISTANCE_METHODS_H
#define DISTANCE_METHODS_H

#include "tree.H"
#include "mytypes.H"

Matrix C(const Matrix& M);

Matrix DistanceMatrix(const Tree& T);

Matrix probability_to_distance(const Matrix &C);

vector<double> FastMTM(const Tree& T,const Matrix& D,
		       const vector<vector<int> >& leaf_set);

vector<double> FastLeastSquares(const Tree& T, const Matrix & D,
				const vector<vector<int> >& leaf_set);

#endif
