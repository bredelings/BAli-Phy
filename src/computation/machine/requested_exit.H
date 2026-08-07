#ifndef REQUESTED_EXIT_H
#define REQUESTED_EXIT_H

// Carries an explicit Haskell exit request through evaluation to the top-level runner.
// This deliberately is not std::exception, because evaluation converts those into runtime errors.
struct requested_exit
{
    int status;
};

#endif
