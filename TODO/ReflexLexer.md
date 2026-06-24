# RE/flex Lexer Experiment

This document summarizes what we learned from the `reflex-lexer-experiment`
work, and sketches a plan for switching the Haskell lexer from Flex to
RE/flex if we decide to keep that direction.

The experiment should be treated as a feasibility branch, not as the final
shape of the lexer.  It showed that RE/flex can generate a working scanner for
the current Haskell lexer, but also exposed places where the current lexer
architecture depends on Flex-specific behavior.

## Goals

The main reasons to consider RE/flex are:

- native Unicode handling;
- a scanner object model that is a better fit for explicit C++ state;
- reducing dependence on Flex-specific empty rules, globals, and macros;
- making it easier to separate raw lexical scanning from Haskell layout.

The goal is not merely to replace one lexer generator with another.  A good
conversion should make the lexer easier to reason about and should reduce the
amount of hidden scanner state.

## What The Experiment Showed

### The Conversion Is Feasible

The current `lexer.l` can be made to compile with RE/flex and can pass the
basic parser, layout, `MultiWayIf`, and `5d +A 50` tests that were run during
the experiment.

This is enough to show that RE/flex is a realistic option.  It is not enough
to show that the conversion is finished or that the current proof-of-concept
should be merged as-is.

### Scanner Object State Needs To Be Reset Explicitly

With Flex, the scanner is effectively driven through generated global scanner
state.  The experiment used a persistent RE/flex scanner object instead.  That
made it necessary to reset the scanner state at the start of each parse:

- clear the scanner start-state stack;
- return to `INITIAL`;
- install the new input buffer.

This is a good kind of explicitness.  It makes lifetime and reuse visible.
However, the final version should avoid a hidden global scanner object if
possible.  Ideally, scanner ownership should be part of the parser/driver
state, or the scanner should be wrapped by an object that has a clear parse
lifetime.

### The Current Layout Rules Depend On Flex Empty Matches

The current Flex lexer uses rules such as empty matches with trailing context
to run layout logic before the next real token is consumed.

RE/flex did not accept the same rules in a directly compatible way.  The
proof-of-concept replaced them with rules that match one byte, roll back the
location, and then push the byte back with `yyless(0)`.

That workaround was useful for proving feasibility, but it is not a good final
design.  It couples layout to scanner backtracking and source-location repair.
It is also fragile: the first version got columns wrong until the location
rollback was added.

### Some Scanner Actions Became Recursive

Some Flex actions that changed scanner state and then used `break` did not
translate cleanly.  In the RE/flex proof-of-concept, some no-token actions
used `return yylex(drv)` to re-enter the scanner under the new state.

That is also a workaround.  It probably has shallow recursion in practice, but
it is still the wrong shape for a final scanner.  A scanner action that does
not produce a token should continue the scanner loop or, better, should not be
needed because layout is handled outside the raw scanner.

### The Lexer Currently Mixes Several Jobs

The lexer currently does all of these at once:

- scans raw tokens;
- tracks source locations;
- tracks beginning-of-line state;
- performs Haskell layout insertion;
- coordinates some parser-sensitive layout behavior;
- manages scanner start states;
- stores or mutates state through generated scanner mechanisms.

This mixing is the main reason the RE/flex conversion needed workarounds.  The
conversion would be cleaner if raw tokenization and layout insertion were
separate layers.

### RE/flex Integration Needs A Real Build Design

The experiment used a temporary `/tmp/re-flex` installation and linked against
that directly.  This is fine for an experiment, but not for a maintained build.

A real conversion needs one of:

- a Meson wrap/subproject that builds the RE/flex tool and library;
- a system dependency lookup for both the tool and library;
- a hybrid approach, where the generated lexer is checked in and only the
  runtime library is needed for normal builds.

Before choosing, we should verify licensing, availability on CI platforms, and
whether the RE/flex runtime library is required for the scanner mode we want.

## Preferred Architecture

The Haskell 2010 layout rule is naturally described as token insertion.  That
suggests this architecture:

```text
source bytes
  -> raw scanner
  -> layout token filter
  -> parser
```

The raw scanner should produce only real tokens and source locations.  The
layout filter should own layout-specific state and may insert virtual `{`, `;`,
and `}` tokens before passing tokens to the parser.

This architecture would remove the need for scanner rules that match nothing,
push input back, or repair locations after probing the next character.

## Layout Filter Design

The layout filter would sit between Bison and the raw scanner.  From Bison's
point of view it would still expose the usual `yylex(driver&)` interface.  From
the scanner's point of view it would repeatedly request raw tokens.

The filter would own:

- the layout stack;
- a queue of pending virtual tokens;
- possibly one pending real token;
- the previous token information needed by layout;
- EOF handling for closing open layout contexts.

When Bison asks for a token, the filter would:

- return a pending virtual token if one exists;
- otherwise fetch or reuse the next real token;
- compare its indentation and token kind to the layout state;
- insert a virtual token if layout requires one before the real token;
- otherwise return the real token.

This is new infrastructure.  The benefit is that the Haskell layout algorithm
becomes ordinary C++ state-machine code, independent of scanner-generator
quirks.  The cost is a new layer between scanner and parser, plus a careful
translation of the current layout behavior.  This is probably worth it if we
commit to RE/flex or Unicode support, but it should be designed and reviewed
before implementation.

## Parser-Error Layout Rule

The delicate part is the Haskell report's parse-error rule for layout.  The
report describes inserting a virtual close brace when doing so would allow
parsing to continue.

A pure token filter can handle indentation-driven layout by itself, but the
parse-error rule needs parser feedback.  There are several possible approaches:

- keep the current Bison error hook mechanism and let it ask the layout filter
  for a virtual close brace;
- implement only the indentation-driven cases at first, then audit whether any
  tests or real inputs rely on parser-error layout insertion;
- expose a narrow callback from the parser to the layout filter for this one
  case.

The final design should be explicit about which of these behaviors it
supports.  We should avoid hiding parser feedback inside scanner macros.

## Unicode Direction

The main long-term reason to use RE/flex is native Unicode handling.  GHC's
lexer uses a byte-class strategy that works with an Alex-style lexer, but it is
described in the source as a practical hack.  For BAli-Phy, it seems better to
aim for a lexer that treats Unicode as part of the scanner's normal input
model.

The first RE/flex conversion does not need to implement every Unicode lexical
class.  A reasonable sequence is:

- get an ASCII-equivalent RE/flex lexer working cleanly;
- add tests for Unicode identifiers and operators;
- map the Haskell lexical classes to RE/flex Unicode character classes;
- compare behavior with GHC on representative inputs.

## Migration Plan

### Keep The Current Experiment As A Reference

The current experiment is useful because it records the mechanical issues that
come up in a direct conversion.  It should remain available on a throwaway or
named experiment branch.

We should not merge the proof-of-concept in its current form because it has
temporary build paths and compatibility workarounds.

### Make Scanner State Explicit Under Flex First

Before changing lexer generators, continue moving scanner state transitions out
of driver helper methods and into visible scanner or layout code.  This work is
valuable even if RE/flex is abandoned.

The desired state is:

- driver methods compute decisions;
- scanner or layout code performs scanner-state transitions;
- hidden generated macros are not used from unrelated helper methods;
- beginning-of-line and layout decisions have visible inputs and outputs.

### Split Raw Scanning From Layout

Add a layout token filter while still using Flex.  This lowers risk because the
lexer generator remains unchanged while the architecture changes.

The first version should preserve current behavior and tests, even if some
code still carries compatibility notes.  Once the filter works, remove the
empty-match layout rules from `lexer.l`.

This step should include tests that cover:

- ordinary layout;
- explicit braces;
- nested layout;
- `do` layout;
- `let` and `where` layout;
- `MultiWayIf`;
- EOF after open layout contexts;
- any parser-error layout behavior that we intentionally keep.

### Add A Real RE/flex Build Integration

After the layout split, replace the temporary `/tmp/re-flex` setup with a real
build integration.

Open questions:

- Can RE/flex be used as a Meson subproject through its CMake build?
- Do we need to link against the RE/flex runtime library for the desired
  scanner mode?
- Are the RE/flex licenses compatible with BAli-Phy distribution?
- Should generated scanner sources remain checked in?
- How should CI install or build the scanner generator on Linux, macOS, and
  Windows?

This should be resolved before the generator switch becomes a normal branch.

### Convert The Raw Scanner To RE/flex

Once layout is no longer encoded through Flex empty matches, the RE/flex
conversion should be much smaller.

At that point, the generated scanner should not need:

- one-byte probe rules;
- `yyless(0)` location rollback for layout;
- recursive `yylex(drv)` calls to force scanner re-entry;
- temporary global scanner objects;
- temporary `/tmp/re-flex` include or library paths.

If any of these are still needed, they should be documented as compatibility
notes with a clear removal path.

### Add Unicode Support Incrementally

After the RE/flex scanner is structurally clean, add Unicode behavior in small
steps:

- Unicode identifier starts and continuations;
- Unicode symbolic operator characters;
- Unicode whitespace if needed;
- tests comparing expected Haskell lexical behavior;
- documentation of known differences from GHC, if any.

This should be done after the scanner switch, not mixed into the initial
generator conversion.

## Suggested Test Set

For each stage, run at least:

- `ninja -C build/gcc-16-debug-O src/bali-phy/bali-phy`;
- parser layout tests such as `parse/18`;
- recursive-do layout tests;
- `MultiWayIf` tests;
- `meson test -C build/gcc-16-debug-O --num-processes 1 "bali-phy:bali-phy 5d +A 50"`.

Before merging a real conversion, run the full parser and Haskell tests, and
at least one external `testiphy` pass if the branch changes generated Haskell
source or model parsing behavior.

## Risks

The main risks are:

- subtly changing Haskell layout behavior;
- losing parser-error layout insertion behavior;
- introducing platform-specific build failures through RE/flex integration;
- treating Unicode differently from GHC in surprising ways;
- leaving behind compatibility workarounds that make the lexer harder to
  maintain.

The way to manage these risks is to separate the architecture cleanup from the
generator switch, and to make layout behavior testable outside of scanner
generator details.

## Recommendation

Do not merge the proof-of-concept conversion directly.

The next useful step is to build a real layout token filter while still using
Flex.  If that works, the later RE/flex conversion should become mostly a raw
scanner replacement rather than a layout refactor and generator switch at the
same time.

This path also gives value if RE/flex is abandoned: the lexer architecture
still becomes more explicit, less macro-dependent, and easier to audit.
