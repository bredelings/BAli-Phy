# RE/flex Lexer Plan

This document records what we learned from the `reflex-lexer-experiment`
branch and lays out a plan for switching the Haskell lexer from Flex to
RE/flex.

The experiment showed that RE/flex is feasible, but it also showed that a
direct mechanical conversion preserves too much of the current Flex-shaped
layout architecture.  The central design issue is not merely which lexer
generator is used.  The central issue is that Haskell layout is currently
implemented with scanner start states and empty Flex rules.

The preferred plan is therefore:

```text
source text
  -> raw scanner
  -> layout token filter
  -> parser
```

The raw scanner recognizes real tokens.  The layout filter schedules real and
virtual tokens.  Parser-visible state changes happen only when a token is
committed to the parser.

Some parts of this design are still incomplete.  In particular, parser-error
layout insertion and the final RE/flex build integration need more design
before implementation.

## Goals

The main reasons to consider RE/flex are:

- native Unicode handling;
- scanner objects and explicit scanner lifetime;
- less dependence on Flex-specific empty rules and macros;
- a cleaner separation between token recognition and Haskell layout insertion;
- a lexer architecture that can be tested without relying on scanner-generator
  quirks.

The goal is not to preserve the current scanner architecture while changing the
generator.  A good conversion should make layout easier to reason about.

## What The Experiment Showed

### The Conversion Is Feasible

The current `lexer.l` can be made to compile with RE/flex and can pass the
basic parser, layout, `MultiWayIf`, and `5d +A 50` tests that were run during
the experiment.

This proves feasibility.  It does not make the proof-of-concept branch a good
final implementation.

### Scanner Object State Must Be Reset Explicitly

The experiment used a persistent RE/flex scanner object.  That made scanner
lifetime visible: each parse had to clear the scanner state stack, return to
`INITIAL`, and install the new input buffer.

This explicitness is good.  The final design should not hide scanner lifetime
behind a global object.  The scanner should either be owned by the driver for a
parse, or owned by a small object with a clear parse lifetime.

### Empty Layout Rules Are The Main Obstacle

The current Flex lexer uses zero-width rules to run layout logic before the
next real token is consumed:

- `<bol>""/.`
- `<layout>""/.`
- `<layout_do>""/.`
- `<layout_if>""/.`
- `<layout_left>""/.`

RE/flex did not accept the same rules in a directly compatible way.  The
proof-of-concept replaced them with one-byte probe rules, location rollback,
and `yyless(0)`.

That workaround should not be kept.  It couples layout to scanner backtracking
and location repair, and it already produced an off-by-one column bug during
the experiment.

### Some No-Token Actions Became Recursive

Some Flex actions changed scanner state and then used `break` to continue
scanning.  In the RE/flex proof-of-concept, some of these became
`return yylex(drv)` to re-enter the scanner under a new state.

This is also a workaround.  The recursion is probably shallow, but the final
design should not rely on recursive scanner entry.  A scanner action that does
not produce a token should either continue a scanner loop normally or disappear
because layout is no longer implemented as scanner states.

### The Current Lexer Mixes Too Many Jobs

The current lexer simultaneously:

- recognizes raw tokens;
- tracks source locations;
- skips whitespace and comments;
- tracks beginning-of-line state;
- inserts Haskell layout tokens;
- manages explicit and implicit layout contexts;
- performs some token classification using previous-token state;
- mutates scanner start states from driver helper methods;
- coordinates with parser error recovery for layout close braces.

This mixing is the reason the RE/flex conversion needed hacks.  The
architecture should be cleaned up under Flex before switching generators.

### Build Integration Is Still Unresolved

The experiment used `/tmp/re-flex` directly.  That is acceptable for a
throwaway branch, but not for a maintained build.

Before a real switch, we need to answer:

- Can RE/flex be used as a Meson subproject through its CMake build?
- Should we prefer a system dependency, a wrap, or both?
- Does the selected RE/flex scanner mode require linking the runtime library?
- Are the RE/flex licenses compatible with BAli-Phy distribution?
- Should generated scanner sources remain checked in?
- How should Linux, macOS, and Windows CI obtain the generator and runtime?

This part of the plan is still vague because it depends on the build-system
choice and license check.

## Current Empty Rule Behavior

The empty rules are not just lexer-generator details.  They are the places
where the current scanner asks layout questions before the next token reaches
the parser.

### Beginning Of Line

Current rule:

```text
<bol>""/.
```

This runs after newlines and trivia, before the first real token on the next
line.  It compares the next token's column with the current layout context:

- if the token is left of the layout column, emit `VCCURLY`;
- if the token is at the layout column and semicolons are enabled, emit `SEMI`;
- otherwise return to normal scanning.

### Post-Keyword Layout

Current rules:

```text
<layout>""/.
<layout_do>""/.
```

These run after layout-introducing keywords such as `let`, `where`, `of`,
`do`, `mdo`, and `rec`.

If the next token is an explicit `{`, the lexer opens an explicit no-layout
context.  Otherwise, the lexer opens an implicit layout context at the next
token's column and emits `VOCURLY`.

### MultiWayIf

Current rule:

```text
<layout_if>""/.
```

After `if`, the lexer checks for the `MultiWayIf` form with a following `|`.
If that form is present, it returns `VBAR` and opens a layout context for the
guard alternatives.  Otherwise it resumes normal scanning.

This case is more delicate than ordinary layout because the current rule uses
lexical context around `|` to avoid confusing it with symbolic operators.

### Immediate Close After Offside Open

Current rule:

```text
<layout_left>""/.
```

If a new implicit layout context would start at or to the left of the previous
layout context, the current lexer emits `VOCURLY` and then immediately emits
`VCCURLY`.  This handles empty or offside layout blocks.

## Proposed Architecture

The parser-facing function remains:

```c++
yy::parser::symbol_type yylex(driver& drv);
```

Internally, it should call a layout filter:

```text
yylex(driver&)
  -> layout_filter.next(driver&, raw_lex)
```

The raw scanner entry point recognizes real tokens:

```text
raw_lex(driver&) -> RawToken
```

The filter may look ahead by asking the raw scanner for one real token, but it
does not immediately commit that token to parser-visible state.  If layout
requires a virtual token first, the filter stashes the real token and returns
the virtual token.

## Raw Token Contract

The raw scanner should return a token description rather than an already
committed parser token.  Conceptually:

```c++
struct RawToken
{
    RawTokenKind kind;
    yy::location loc;
    std::string text;
    TokenPayload payload;
    bool starts_line;
    bool has_leading_space;
    bool followed_by_opening;
    LayoutIntent layout_after;
};
```

The exact names and representation are not fixed.  The contract is the
important part.

`starts_line` means that the token is the first real token after one or more
newlines and intervening trivia.  The filter should use it for ordinary BOL
offside checks only when there is no pending post-keyword layout intent.
Pending layout intent has priority because the current `<layout>` and
`<layout_do>` scanner states consume newlines and open the new layout context
before ordinary `<bol>` handling can run.

The raw scanner may:

- consume source input;
- skip whitespace and comments;
- compute source locations;
- decode strings, characters, and numeric literals;
- classify lexemes that do not depend on parser-visible token history;
- record spacing facts;
- record that a keyword requests layout after it.

The raw scanner must not:

- emit virtual layout tokens;
- push or pop layout contexts;
- mutate previous-token state used for operator classification;
- classify operators using state that may change before the token is delivered;
- push Flex or RE/flex start states for layout;
- perform effects that must happen in parser token order.

This is stricter than the current scanner.  The scanner currently performs
several parser-visible effects in token actions, and those effects must move
to commit time.

## Commit Contract

A token is committed when the layout filter returns it to Bison.

Commit-time code should perform parser-visible effects in the exact order that
the parser sees tokens.  These effects include:

- pushing a no-layout context for an explicit `{`;
- popping a context for an explicit `}`;
- pushing an implicit layout context when returning `VOCURLY`;
- popping an implicit layout context when returning `VCCURLY`;
- marking whether the last committed token was closing;
- classifying reserved symbolic operators that depend on previous-token state;
- recording layout intent after a committed keyword.

There is one deliberate exception: the parser-error layout rule can close an
implicit layout context through a narrow parser-to-filter API.  This is still a
parser-visible layout effect, but it is triggered by Bison error recovery
rather than by the filter returning a token.

This is the key correctness rule:

```text
Recognizing a real token may happen early.
Committing that token must happen only when the parser receives it.
```

This rule prevents a delayed token from mutating state before an inserted
`VOCURLY`, `SEMI`, or `VCCURLY`.

### Virtual Token Commit Table

Virtual and layout-sensitive tokens should have explicit commit effects:

| Token or event | Commit effect |
| --- | --- |
| `VOCURLY` | Push one implicit layout context with the chosen indentation column and semicolon policy, mark the token as non-closing, and return `VOCURLY` to the parser. |
| `VCCURLY` | Pop one implicit layout context, mark the token as closing if operator classification needs virtual close braces to behave like closing delimiters, and return `VCCURLY` to the parser. |
| `SEMI` | Do not mutate the layout stack, mark the token as non-closing, and return `SEMI` to the parser. |
| explicit `{` | Verify that an explicit block is legal at this location, push one no-layout context, mark the token as non-closing, and return `{` to the parser. |
| explicit `}` | Pop one layout/no-layout context, mark the token as closing, and return `}` to the parser. |
| parser-error close | Through the narrow parser-to-filter API, pop one implicit layout context and let Bison recover as if the `close` nonterminal had been satisfied. |

The `VCCURLY` previous-token effect is a design point to verify during the
operator-classification audit.  The table should be updated if the
`prec_close_count` replacement shows that virtual close braces should not count
as closing tokens.

The explicit `{` legality check should preserve the current
`hopefully_open_brace()` behavior.  If there is an enclosing implicit layout
context, the explicit `{` must occur to the right of that context's indentation
column.  Otherwise the parser should report the current "Missing block" error
instead of pushing a no-layout context.

## Side Effects That Must Move

The current scanner actions contain effects that are not safe if the token is
recognized early and delivered later.

These should move to commit time or become raw-token metadata:

- `drv.push_context()` for explicit `{`;
- `drv.pop_context()` for explicit `}`;
- `drv.set_closing_token()`;
- `drv.step_closing_token()` in `YY_USER_ACTION`;
- `drv.check_closing_token()` inside `varsym()`;
- `driver::varid()` pushing `layout`, `layout_do`, or `layout_if` Flex states.

The old countdown-based `prec_close_count` must be replaced by explicit
previous-token and spacing metadata before the filter starts delaying real
tokens.  The current direction is:

```text
operator classification =
  previous committed token was closing
  + current raw token has leading whitespace or not
  + current raw token is followed by an opening token or not
```

This is a required design checkpoint, not a vague cleanup.  It needs a focused
audit before implementation because it affects prefix, tight infix, loose
infix, and reserved-symbol classification.

## Filter State

The layout filter should own or explicitly access:

- one pending real token;
- a queue of pending virtual tokens;
- the layout context stack;
- pending layout intent from the last committed keyword;
- previous committed token information;
- EOF layout-closing state;
- a parser-error close method used only by the grammar's `close: error`
  production.

The layout stack should move into the layout filter, or be owned by an object
that the filter controls.  Code outside the filter should not mutate it
directly.  The parser may request a parse-error close through the filter's
narrow API, but it should not call `drv.pop_context()` itself.

## Filter Algorithm

Each parser-facing `yylex(driver&)` call should follow this shape:

1. If a virtual token is queued, commit and return it.
2. If no real token is pending, call the raw scanner.
3. If there is ordinary pending layout from a committed keyword, such as
   `layout` or `layout_do`, handle that before ordinary BOL/offside logic:
   - for ordinary layout keywords, if the real token is explicit `{`, first
     run the explicit-brace legality check and then commit the explicit brace;
   - otherwise open an implicit context at the real token's column, stash the
     real token, and return `VOCURLY`;
   - if the new column is not to the right of the enclosing layout context,
     queue `VCCURLY` immediately after `VOCURLY`.
4. If the pending layout intent is the `if`/`MultiWayIf` case, handle the
   leading `|` case specially before ordinary BOL/offside logic.  If the next
   real token is not a layout-leading `|`, clear the pending `if` intent and
   commit that token normally; do not run ordinary BOL/offside handling for
   newlines that were consumed while resolving the pending `if`.
5. If there is no pending layout intent and the real token starts a new
   physical line, run the offside check:
   - if the token is left of the current layout column, pop the layout context
     and return `VCCURLY`;
   - if the token is at the layout column and semicolons are enabled, return
     `SEMI`;
   - otherwise continue.
6. Otherwise commit and return the pending real token.

This is still a design sketch.  The final implementation should turn this into
smaller named decisions with tests around each migrated empty-rule category.

## Equivalence With Current Behavior

The reason this can preserve behavior is that each current empty rule maps to
a filter decision using the same inputs.

### Beginning Of Line Equivalence

Current scanner behavior:

```text
newline/trivia -> enter bol state
before next token -> compare next token column with layout stack
```

Filter behavior:

```text
raw scanner returns first real token with starts_line=true
if there is no pending post-keyword layout, filter compares that token's
column with layout stack
```

The parser-visible token stream is the same, as long as the raw scanner marks
the first real token after newline/trivia accurately and the filter gives
pending post-keyword layout priority over ordinary BOL handling.

### Post-Keyword Layout Equivalence

Current scanner behavior:

```text
return keyword and push layout state
before next token -> emit VOCURLY or consume explicit {
```

Filter behavior:

```text
commit keyword and record pending layout intent
raw-read next token without committing it
emit VOCURLY or commit a legal explicit {
```

The parser sees the same order.  The important difference is that the raw next
token's side effects are delayed until after any inserted virtual token.

### Immediate Close Equivalence

Current scanner behavior:

```text
emit VOCURLY
enter layout_left state
emit VCCURLY before next token
```

Filter behavior:

```text
emit VOCURLY
queue VCCURLY
then deliver the stashed real token
```

This avoids a scanner state whose only purpose is to emit a zero-width virtual
token.

### MultiWayIf Equivalence

Current scanner behavior:

```text
after if, inspect whether the next lexical item is a leading |
```

Filter behavior:

```text
after committing if, inspect the next raw token
```

This should be equivalent, but it is less settled than the other cases because
the current rule uses raw lexical context around `|`.  The implementation plan
should migrate this last and add focused tests.

## Parser-Error Layout Rule

The Haskell report describes inserting a virtual close brace when doing so
would allow parsing to continue.  A pure token filter can handle ordinary
indentation-driven layout, but this parse-error rule needs parser feedback.

This is a real layout rule, not just an error-reporting detail.  For example:

```haskell
main = let x = 1 in x
```

The token `in` is not offside.  It is on the same line and to the right of the
layout column introduced by `let`.  A purely indentation-based filter would
not know to close the `let` block before `in`.

GHC documents another grammar-sensitive example:

```haskell
f x = case x of
  True -> False
  where y = x + 1
```

The virtual close brace must be inserted before `where`, but that cannot be
decided from indentation alone.  The parser has to discover that parsing would
fail inside the current layout block and then close the block.

The current grammar has:

```text
close: VCCURLY
     | error { yyerrok; drv.pop_error_message(); drv.pop_context(); }
```

This mirrors GHC's practical split: ordinary indentation-driven layout happens
in the lexer, while grammar-sensitive layout close happens through the
parser's `error` token in the `close` production.

### Parser-To-Filter Close API

The layout filter should own layout stack mutation, but it should expose one
explicit parser-error close method.  The grammar action should become
conceptually:

```c++
close: VCCURLY
     | error { yyerrok; drv.layout_filter.close_for_parse_error(); }
```

The actual spelling can differ, but the ownership should be clear: the parser
requests a grammar-sensitive layout close, and the filter performs the layout
state mutation.

The layout-close method should:

- verify that the top context is an implicit layout context;
- pop exactly one implicit layout context;
- leave the current lookahead token available for normal parsing after the
  recovered `close`;
- be used only from the `close: error` production.

The grammar action or a small driver wrapper should also suppress the
speculative parse error message recorded by Bison.  That diagnostic cleanup is
part of preserving current behavior, but it does not have to live inside the
layout filter itself.

This is new infrastructure.  The benefit is that it preserves the Haskell
report behavior and current BAli-Phy/GHC behavior while keeping layout stack
ownership explicit.  The cost is a narrow grammar-to-filter callback.  The
interface is the right shape because it exposes only the one grammar-sensitive
operation required by layout, instead of letting parser actions mutate layout
state directly.

### Compatibility Note

This parser-to-filter callback is intentionally grammar-dependent.  It exists
because the Haskell layout rule cannot be implemented from indentation alone.
It should remain small and isolated.  A more elaborate parser/filter retry
mechanism could replace it later, but that would be more complex than the
current migration needs.

## Implementation Plan

### Start A Separate Branch

Create a dedicated jj line of work for the RE/flex transition.  The old
experiment should remain a reference, not the base implementation.

Suggested bookmark name:

```text
reflex-lexer
```

### Add Or Confirm Layout Tests

Before changing architecture, add tests for the layout behavior that currently
depends on empty rules:

- beginning-of-line virtual semicolon;
- beginning-of-line virtual close brace;
- nested `let` and `where`;
- `of` alternatives;
- `do`, `mdo`, and `rec`;
- explicit braces after layout-introducing keywords;
- immediate virtual close after offside layout open;
- `MultiWayIf`;
- EOF after open layout contexts;
- any parser-error layout behavior we intentionally preserve.

### Make Scanner-State Decisions Explicit Under Flex

Recover the useful cleanup from the experiment branch: driver helper methods
should compute layout decisions, not push or pop Flex states.

In particular, `driver::varid()` should not push `layout`, `layout_do`, or
`layout_if`.  It should classify the keyword and return layout intent as data.

This step has value even if RE/flex is later abandoned.

### Add Raw Token And Commit-Time Effects

Introduce a raw-token representation and a commit path while still preserving
current behavior.

This is new infrastructure and needs design review before implementation.  The
benefit is that token recognition and parser-visible token effects become
separate.  The cost is a new internal token type and a commit function.  The
interface is justified only if it is used immediately by the parser-facing
lexer path.

The first commit should avoid large behavior changes.  It can route tokens
through the new commit path without delaying tokens yet.

### Replace Previous-Token Countdown State

Replace `prec_close_count` with explicit previous-token and spacing metadata
before any real tokens can be delayed behind virtual layout tokens.

The replacement design should specify:

- which committed token kinds count as closing tokens;
- whether virtual `VCCURLY` counts as closing for operator classification;
- how the raw scanner records leading whitespace before a symbol;
- how the raw scanner records whether a symbol is followed by an opening
  token;
- where reserved-symbol classification happens after the replacement.

This checkpoint should include tests for prefix, loose infix, tight infix, and
reserved-symbol classification.  It is required because delayed-token layout
insertion would otherwise observe previous-token state in the wrong order.

### Move Beginning-Of-Line Layout Into The Filter

Replace `<bol>""/.` first.  This is the cleanest empty-rule category.

The raw scanner should mark the first real token after newline/trivia with
`starts_line=true`.  The filter should use that token's location to insert
`VCCURLY` or `SEMI` before committing the real token.

### Move Post-Keyword Layout Into The Filter

Replace `<layout>""/.` and `<layout_do>""/.`.

A committed layout keyword records pending layout intent.  The filter then
uses the next raw token either to commit an explicit `{` or to insert
`VOCURLY` before the next real token.

### Move Immediate Close Into The Filter

Replace `<layout_left>""/.`.

When an implicit context would open at or left of the enclosing layout column,
the filter should return `VOCURLY`, queue `VCCURLY`, and then deliver the
stashed real token.

### Move MultiWayIf Last

Replace `<layout_if>""/.` only after the ordinary layout cases are stable.

This step needs focused tests because `|` can also be an operator.  The filter
should ideally reason from raw token kind and spacing metadata rather than
from scanner trailing context.

### Route Parser-Error Close Through The Filter

Replace direct grammar-side layout mutation:

```c++
drv.pop_context()
```

with a narrow parser-to-filter API such as:

```c++
drv.layout_filter.close_for_parse_error()
```

This should preserve the current `close: error` behavior while making layout
stack ownership explicit.  The method should also handle the current
`drv.pop_error_message()` behavior, or the grammar should call a clearly named
error-suppression helper next to the layout close.

Add tests for grammar-sensitive layout closes such as:

```haskell
main = let x = 1 in x
```

and a case/where interaction where indentation alone is not enough to decide
that the layout block must close.

### Remove Obsolete Flex Start States

After all empty layout rules are gone, remove the layout-specific Flex start
states:

- `bol`;
- `layout`;
- `layout_do`;
- `layout_if`;
- `layout_left`.

Nested comment and pragma states may remain in the raw scanner.

### Add Real RE/flex Build Integration

Replace the temporary `/tmp/re-flex` setup with a real build design.

This step should not happen until the layout-specific Flex states have been
removed or reduced enough that the conversion does not require probe rules and
location rollback.

### Convert Raw Scanner To RE/flex

Switch the raw scanner from Flex to RE/flex.

At this stage, the generated scanner should not require:

- one-byte layout probe rules;
- `yyless(0)` layout rollback;
- recursive `yylex(drv)` calls;
- layout-specific start states;
- temporary global scanner objects;
- temporary `/tmp/re-flex` include or library paths.

Any remaining compatibility workaround should have a short note explaining why
it exists, what it does, and what needs to happen before it can be removed.

### Add Unicode Support Incrementally

After the RE/flex scanner is structurally clean, add Unicode behavior in small
steps:

- Unicode identifier starts and continuations;
- Unicode symbolic operator characters;
- Unicode whitespace if needed;
- tests comparing expected Haskell lexical behavior;
- notes on known differences from GHC.

Unicode support should not be mixed into the initial generator switch.

## Unicode Direction

The main long-term reason to use RE/flex is native Unicode handling.  GHC uses
a byte-class strategy in its lexer that works in practice, but that approach is
not the direction we want to copy unless RE/flex proves unsuitable.  For
BAli-Phy, the preferred direction is to let the lexer treat Unicode as part of
its normal input model.

The first RE/flex conversion should be ASCII-equivalent.  Unicode support
should be added after the scanner switch is structurally clean, with tests for
identifiers, symbolic operators, and any whitespace classes we decide to
support.

## Incomplete Design Areas

Several parts of this plan are intentionally not fully designed yet:

- the exact `RawToken`, `TokenPayload`, and `LayoutIntent` representation;
- whether the layout stack lives in `driver` or in the layout filter;
- how `MultiWayIf` should distinguish a layout-leading `|` from symbolic
  operator syntax;
- the exact spelling and ownership of the parser-to-filter
  `close_for_parse_error` API;
- how RE/flex should be integrated into Meson and CI;
- how closely Unicode behavior should match GHC in edge cases.

These should be resolved before the relevant implementation step.  In
particular, the raw-token and commit-time effect contracts should be reviewed
before adding new infrastructure.

## Suggested Commit Structure

One possible commit sequence:

1. `tests: cover Haskell layout edge cases`
2. `parser: make layout keyword intent explicit`
3. `parser: route tokens through a commit step`
4. `parser: replace previous-token countdown state`
5. `parser: move BOL layout into token filter`
6. `parser: move keyword layout into token filter`
7. `parser: queue immediate offside layout close`
8. `parser: move MultiWayIf layout into token filter`
9. `parser: route parse-error layout close through filter`
10. `parser: remove layout-specific Flex states`
11. `build: add RE/flex dependency`
12. `parser: generate Haskell lexer with RE/flex`
13. `parser: add Unicode lexer coverage`

This structure may change as implementation reveals smaller natural commits.
The important rule is that new infrastructure should be used immediately and
old layout paths should be deleted as soon as their replacement is working.

## Suggested Test Set

For each stage, run at least:

```sh
ninja -C build/gcc-16-debug-O src/bali-phy/bali-phy
meson test -C build/gcc-16-debug-O --num-processes 1 "bali-phy:bali-phy testsuite parse/18"
meson test -C build/gcc-16-debug-O --num-processes 1 "bali-phy:bali-phy 5d +A 50"
```

Also run the `MultiWayIf` and recursive-do tests after the relevant steps.

Before merging a real RE/flex conversion, run the broader parser and Haskell
tests, and at least one external `testiphy` pass if parsing or generated
Haskell behavior changed.

## Risks

The main risks are:

- changing Haskell layout behavior subtly;
- committing token side effects in the wrong order when a real token is
  delayed behind a virtual token;
- losing parser-error layout behavior;
- changing prefix/tight-infix operator classification;
- introducing platform-specific build failures through RE/flex integration;
- treating Unicode differently from GHC in surprising ways;
- leaving behind compatibility workarounds that make the lexer harder to
  maintain.

The side-effect ordering risk is the most important new design point.  The
filter must delay token commit, not merely delay returning an already-committed
token.

## Current Recommendation

Do not merge the proof-of-concept conversion directly.

The next implementation step should be under Flex: make token recognition,
layout scheduling, and token commit separate enough that the empty layout rules
can be removed one group at a time.

Once the layout-specific empty rules are gone, switching the raw scanner to
RE/flex should be a much smaller and cleaner change.

This path still provides value if RE/flex is abandoned, because the lexer
architecture becomes more explicit and less dependent on Flex-specific
behavior.
