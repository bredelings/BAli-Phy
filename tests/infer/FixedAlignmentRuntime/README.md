Run checks early C++ rejection of fixed-alignment MCMC before creating an output directory.
The separate Meson test "generated fixed-alignment rejection" checks successful generation
in test mode and rejection by the retained Haskell program, so a second success-only test
is unnecessary. These checks become obsolete if fixed alignments become valid during MCMC.
