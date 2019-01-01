#ifndef INDEX_VAR_H
#define INDEX_VAR_H

/// a de Bruijn indexed variable 
struct index_var
{
    int index;
    index_var(int i):index(i) {}
};

#endif
