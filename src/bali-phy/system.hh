#ifndef STARTUP_SYSTEM_H
#define STARTUP_SYSTEM_H

#include <iostream>

void raise_cpu_limit(std::ostream& o);

void block_signals();

#endif
