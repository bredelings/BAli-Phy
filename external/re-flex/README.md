# Bundled RE-flex scanner runtime

Upstream: https://github.com/Genivia/RE-flex
Release: 6.3.0
Archive: https://github.com/Genivia/RE-flex/archive/refs/tags/v6.3.0.tar.gz
SHA-256: c9e448b621734238c22352f54562bb88c1c6c18450f89d8fe6a2eb2bcce68a2b

This directory contains the seven runtime sources listed in upstream's
“Minimized library and (cross) compiling from source” instructions:
debug.cpp, error.cpp, input.cpp, matcher.cpp, pattern.cpp, utf8.cpp, and simd.cpp.
The complete include/reflex header directory is retained to simplify updates.
Generator sources and generation-time Unicode tables are not included.

Meson builds a private static library using the portable path, without explicit SIMD
feature flags. No system RE-flex package or download is needed. Headers and the library
are not installed separately; LICENSE.txt is installed under share/doc/bali-phy/licenses/re-flex.
The target undefines BAli-Phy's HAVE_CONFIG_H to preserve RE-flex's platform defaults.

## Local modification

lib/input.cpp retains the MinGW file-I/O fix from the former Meson wrap:
MinGW uses Windows file APIs instead of the POSIX select path, whose header is unavailable.
Remove this modification when the upstream release being imported handles MinGW correctly.
Upstream source files are otherwise unchanged.

## Regenerating the scanner

Ordinary builds compile the checked-in generated scanner; they do not run the generator.
Install RE-flex 6.3.0 and Bison separately, then run from the checkout root:

    cd src/computation/parser
    ./gen_parser.sh

The script accepts REFLEX=/path/to/reflex to select the generator. Review changes to the
generated scanner and parser before committing. A generator upgrade must be checked against
the bundled runtime; do not assume arbitrary generator/runtime versions are compatible.

## Updating the runtime

1. Download and verify an upstream release. Record its version, URL, and checksum here.
2. Replace the seven lib sources, include/reflex directory, and LICENSE.txt from that release.
   Check upstream's runtime source list for changes.
3. Review and reapply the MinGW fix only if still needed.
4. Update the Meson version summary and check compatibility with the generated scanner.
5. Compile and install the full project, run the lexer tests and 5d +A test, and cross-build
   with MinGW. Check source packaging and installed license contents.

Keep upstream imports and local patches in separate commits. The vendored code is BSD-3-Clause;
retain its copyright and license notices.
