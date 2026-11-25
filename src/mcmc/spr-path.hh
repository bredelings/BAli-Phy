#ifndef MCMC_SPR_PATH_HH
#define MCMC_SPR_PATH_HH

#include "util/assert.hh"
#include <cstdint>

// The tip-to-tip path has N internal nodes. Suppressing the attachment at node A leaves
// N branches numbered 1..N. Each occurs twice on the directed cycle (1,...,N,N,...,1):
// one step at a tip reverses the terminal branch, so every walk has an equally likely reverse.
inline int spr_path_target_edge(int N, int A, bool forward, std::int64_t steps)
{
    assert(1 <= A and A <= N and steps >= 0);
    const std::int64_t period = 2LL * N;
    const auto start = forward ? A - 1 : period - A;
    const auto end = (start + steps % period) % period;
    const int C = static_cast<int>(end < N ? end + 1 : period - end);

    // Return the left node index of the edge on the original path. At C == A the
    // right-hand segment represents the fused edge and invokes the current-attachment update.
    return C < A ? C - 1 : C;
}

#endif
