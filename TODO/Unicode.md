# Unicode Support Plan

## Purpose

This document describes staged Unicode support for BAli-Phy's Haskell
implementation.  The target is UTF-8 Haskell source input, Unicode scalar
`Char` values, Unicode identifiers and operators, and exact byte-sequence
identifier equality after validation.

The plan is intentionally incremental.  Some layers can be made Unicode-aware
before the runtime is fully Unicode-aware, but every temporary boundary must be
explicit and must reject values it cannot represent rather than silently
truncate them.

## Current Status

Done:

- `Hs::Char` stores a Unicode scalar candidate using `char32_t`.
- `Literal::is_Char()` returns `std::optional<char32_t>`.
- Parser semantic values for `CHAR` and `PRIMCHAR` use `char32_t`.
- The lexer helper for character literals returns `char32_t`.
- Desugaring rejects `Hs::Char` values that cannot fit in the current
  byte-sized runtime `Char`.
- ASCII lexer regression tests cover character/string literals and `--`
  comment behavior versus symbolic operators.
- The lexer parses decimal numeric escapes in character and string literals.
- The lexer rejects `\&` in character literals and treats it as an empty escape
  in string literals.
- `Hs::String::print()` escapes quotes, backslashes, controls, and bytes that
  are not safe printable ASCII.
- String printing is byte-preserving for now: bytes greater than `0x7f` print
  as numeric escapes until Runtime/Text are deliberately widened.
- Literal escape tests cover decimal escapes, `\&`, invalid scalar escapes, and
  the current byte-sized runtime/string guards.

Still limited:

- The lexer accepts only ASCII character and string literal source today.
- `Core::Constant` and `Runtime::Char` still store byte-sized characters.
- `Runtime::Exp::as_char()` and related closure constructors still assume
  byte-sized characters.
- `Data.Char`, `Data.Text`, `Text.pack`, `Text.unpack`, `hPutChar`,
  `hGetChar`, `intToChar`, `charToInt`, and `integerToChar` still need
  Unicode scalar or UTF-8 semantics.
- `haskell/ids.cc` still classifies identifiers and symbols byte-by-byte using
  ASCII rules.
- `computation/parser/lexer.l` still has placeholder Unicode character classes
  such as `unilarge`, `unismall`, `unisymbol`, and `uniidchar`.
- Some `UnicodeSyntax` reserved-symbol mappings already exist in the parser
  driver, but the lexer does not yet recognize those Unicode symbols as normal
  Haskell operator tokens.

Partial ASCII baseline coverage exists, but the full baseline checklist before
large lexer changes is:

- ASCII character and string literals.
- Escaped character and string literals.
- ASCII identifiers and qualified identifiers.
- ASCII symbolic operators.
- `--` comment behavior versus `--+` or `-->` operator behavior.
- Layout with ASCII identifiers.

## Invariants

- Source input is UTF-8.
- Identifier equality is exact UTF-8 byte-sequence equality after validation.
- Identifier normalization is not performed initially.
- Invalid UTF-8 is rejected at the boundary where it is interpreted as
  Unicode.
- Unicode scalar values exclude surrogate code points and values greater than
  `0x10ffff`.
- One Unicode code point counts as one source column for parser diagnostics and
  layout decisions.  Combining marks are still counted as one code point unless
  a later design explicitly moves to display-width columns.
- Tabs keep the current tab/location behavior unless a later focused change
  decides otherwise.
- Decimal numeric literals remain ASCII-only even if Unicode `Nd` characters
  are allowed inside identifiers.
- Temporary byte-sized runtime boundaries must be marked with compatibility
  notes and must reject unrepresentable values.

## Design Decisions

- Keep `Hs::Char` as `char32_t`.
- Keep `Hs::String` as `std::string` containing UTF-8 bytes.
- Use RE/flex Unicode tables for Unicode category checks unless a concrete need
  justifies ICU, libunistring, or another Unicode dependency.
- Keep UTF-8 and category helpers private to `haskell/ids.cc` unless another
  concrete caller appears.
- Treat `UnicodeSyntax` as separate from general Unicode identifiers and
  operators.
- Do not normalize identifiers at first.  This keeps source names predictable
  and avoids introducing normalization tables or policy before they are needed.

## Near-Term Literal Work

### Numeric Character Escapes

Status: implemented for decimal escapes.

The Haskell source printer can emit numeric character escapes for non-ASCII or
non-printable `Hs::Char` values.  The lexer should parse the same syntax before
we rely on printed Haskell source round-tripping.

Required behavior:

- Parse decimal numeric escapes in character literals.
- Parse decimal numeric escapes in string literals.
- Reject numeric escapes that are not Unicode scalar values.
- Reject numeric escapes that cannot currently be represented by the byte-sized
  runtime when desugaring is reached.
- Handle escape termination unambiguously.  If a numeric escape is followed by
  a digit in a string literal, the printer must use a Haskell empty escape
  `\&` or another unambiguous representation.

Validation:

- Existing ASCII literal tests.
- Character literal with a numeric ASCII escape.
- String literal containing a numeric escape before a following digit.
- Invalid numeric escape above `0x10ffff`.
- Surrogate numeric escape rejected.

### Safe `Hs::String` Printing

Status: implemented with byte-preserving semantics.

`Hs::String` remains UTF-8 bytes, but printing should be deliberate.

Required behavior:

- Escape quotes, backslashes, and standard control characters.
- Escape other control bytes using numeric escapes.
- Print bytes greater than `0x7f` as numeric escapes until generated Haskell
  source and Runtime/Text intentionally switch to Unicode text semantics.
- Do not interpret or validate UTF-8 here yet; this printer is byte-preserving
  until Runtime/Text follow-through changes that policy.
- Avoid numeric escape ambiguity by inserting `\&` when needed.

Validation:

- Quotes and backslashes in generated string literals.
- Newline, tab, carriage return, and other control bytes.
- Numeric escape followed by a digit.
- Bytes greater than `0x7f` print as numeric escapes.

### Character Literal Semantics

Already implemented:

- `Hs::Char` can store `char32_t`.
- Standard ASCII escapes are preserved when printing.
- Printable ASCII prints directly.
- Non-ASCII and non-printable code points print as numeric escapes.
- Invalid scalar values are rejected by the character literal printer.

Remaining work:

- Optional direct C++ tests for constructed non-ASCII and invalid `Hs::Char`
  printing if a convenient local test hook is added.
- Runtime/Text follow-through before non-byte `Char` values can be desugared.

## Runtime Char and Text Follow-Through

Unicode source support and Unicode runtime character support are related but
not identical.  The current source AST can represent wider characters, but the
runtime cannot.

Suggested order:

1. Keep the current desugar-time byte guard while parser/source work is still
   ASCII-only or while non-byte chars are only printable source objects.
2. Widen `Core::Constant` character storage from `char` to a Unicode scalar
   type.
3. Widen `Runtime::Char` and `Runtime::Exp::as_char()`.
4. Update closure constructors and conversions between Core constants and
   runtime atoms.
5. Update primitive/builtin functions that consume or produce `Char`.
6. Update `Data.Text` and text IO to treat text as UTF-8 with explicit
   encode/decode boundaries for `[Char]`.
7. Remove the desugar-time byte guard once the runtime can represent all
   Unicode scalar values.

Known consumers to audit:

- `Core::Constant`
- `Runtime::Char`
- `Runtime::Exp::as_char()`
- closure constructors for character values
- `runtime_exp_from_core_constant()`
- `core_constant_from_runtime_exp()`
- `atom_from_constant()`
- `runtime_constant_to_core()`
- `Data.Char` builtins
- `Data.Text` builtins
- `Text.pack`
- `Text.unpack`
- `hPutChar`
- `hGetChar`
- `intToChar`
- `charToInt`
- `integerToChar`
- `charToInteger`

## UTF-8 Identifier Classification

### Design Analysis

Adding UTF-8 iteration and Unicode category lookup is infrastructure.  The
benefit outweighs the cost because qualifier splitting, identifier validation,
symbol validation, and lexer-rule validation need consistent Unicode decoding
and category rules.  The interface should stay narrow and private at first:
helpers in `haskell/ids.cc` should expose only the decisions needed by existing
identifier predicates, not a general Unicode library.

The cost is a new dependency from normal Haskell identifier validation code to
RE/flex Unicode tables.  Before implementing this stage, verify that the RE/flex
Meson subproject exposes the needed headers and objects cleanly to
`libcomputation` on Linux, macOS if applicable, and Windows/mingw.

### UTF-8 Iteration

Add private helper code inside `haskell/ids.cc`:

- Iterate over `std::string_view`.
- Yield `{char32_t codepoint, size_t begin_byte, size_t end_byte}`.
- Treat invalid UTF-8 as invalid identifier input.
- Use byte offsets for substrings, so module qualifier splitting remains valid.
- Use RE/flex UTF-8 decoding if suitable; otherwise write a small validating
  decoder with tests.

### Unicode Category Membership

Add a helper around RE/flex Unicode ranges:

- Ranges are `[lo, hi]` integer pairs ending in `0, 0`.
- Add `range_contains("Lu", cp)` or an equivalent private helper.
- Cache range pointers or use small static accessors to avoid repeated lookup
  noise.
- Test category membership for representative code points if practical.

### Classification Rules

Replace byte-based classification with code-point classification:

- large: ASCII `A-Z`, Unicode `Lu`, `Lt`
- small: ASCII `a-z`, `_`, Unicode `Ll`, `Lo`
- identifier continuation: small, large, ASCII digits, Unicode `Nd`, `Nl`,
  `No`, `Lm`, `Mn`, and `'`
- symbol: ASCII symbol chars plus Unicode `Pc`, `Pd`, `Po`, `Sm`, `Sc`, `Sk`,
  `So`

Clarifications:

- `_` remains a varid-start character even though Unicode connector
  punctuation may also be symbol-like in other contexts.
- `:` remains the only consym-start character.
- Unicode decimal digits are identifier continuations, not numeric literal
  digits.
- Builtin constructor names such as `[]`, `()`, and tuple constructors keep
  their existing special handling.

Update these functions:

- `is_haskell_id`
- `is_haskell_varid`
- `is_haskell_conid`
- `is_haskell_uqsym`
- `is_haskell_varsym`
- `is_haskell_consym`
- `skip_conid_dot`
- `find_module_separator`
- `haskell_name_path`
- `get_haskell_identifier_path`

Validation:

- Unicode lowercase varids.
- Unicode uppercase/titlecase conids.
- Unicode symbolic operators.
- Unicode qualified names.
- Invalid UTF-8 rejected.
- Invalid identifier code points rejected.
- Module qualifier splitting with Unicode components.
- Existing ASCII identifier and operator tests still pass.

## Source Locations and Layout

Before changing lexer rules, verify RE/flex Unicode location behavior:

- What do `matcher().columno()` and `matcher().columno_end()` report in
  `%option unicode` mode?
- Are columns bytes or code points?
- How are tabs counted relative to current behavior?

Required invariant:

- Parser locations and layout indentation use code-point columns, not UTF-8
  byte offsets.
- Byte offsets may still be used internally for slicing `std::string` source
  text.

If RE/flex reports byte columns in Unicode mode, add a small location-update
helper that computes code-point columns for matched text.  That helper is new
infrastructure and needs a focused design analysis before implementation.

Validation:

- Error span for a token after a non-ASCII identifier on the same line.
- Layout indentation after non-ASCII identifiers.
- Existing ASCII layout tests.

## Lexer Unicode Mode and Categories

Replace placeholder classes in `computation/parser/lexer.l`:

- `unilarge`
- `unismall`
- `unidigit`
- `unisymbol`
- `unispace`
- `unigraphic`
- `uniidchar`

Use RE/flex Unicode property classes matching the `haskell/ids.cc`
classification rules.  Keep decimal numeric literals ASCII-only.

Add `%option unicode` and regenerate scanner sources using
`computation/parser/gen_parser.sh`.

Generated files to check:

- `lexer.cc`
- `parser.cc`
- `parser.hh` when parser semantic types or grammar changed

Validation:

- Build `src/bali-phy/bali-phy`.
- Focused parser/lexer tests.
- Existing ASCII lexer tests.
- Representative Haskell parser tests.

## Operators and Comments

Add or verify lexer behavior for:

- `--` followed by a Unicode symbol should be a varsym, not a comment.
- Unicode symbol operators adjacent to identifiers.
- Prefix, suffix, loose infix, and tight infix classification with Unicode
  identifier starts after an operator.
- `varsym` lookahead must include Unicode opening lexeme cases, not only ASCII
  starts.

Validation:

- Unicode symbolic operator definition and use.
- `--` followed by a Unicode symbol operator.
- Unicode operator before an identifier with no intervening space.
- Unicode operator after an identifier with no intervening space.

## Unicode Character and String Source Parsing

After lexer Unicode mode and `Hs::Char` support are stable:

- Parse raw non-ASCII char literals as `char32_t`.
- Parse raw non-ASCII string literal content as UTF-8.
- Keep escape behavior explicit and tested.
- Reject malformed UTF-8 with a lexer/parser diagnostic.

Validation:

- Raw non-ASCII character literal.
- Raw non-ASCII string literal.
- Malformed UTF-8 in a character literal.
- Malformed UTF-8 in a string literal.
- Existing ASCII literal tests.

## UnicodeSyntax

Some `UnicodeSyntax` reserved-symbol mappings already exist in `driver.cc`.
The remaining work is to ensure the lexer recognizes the Unicode symbols as
operator tokens and that extension gating behaves correctly.

Mappings to support:

- `∷` -> `::`
- `⇒` -> `=>`
- `∀` -> `forall`
- `→` -> `->`
- `←` -> `<-`
- `⤙` -> `-<` when arrows are enabled
- `⤚` -> `>-` when arrows are enabled
- `⤛` -> `-<<` when arrows are enabled
- `⤜` -> `>>-` when arrows are enabled
- `★` -> `*` when `StarIsType` applies

Keep this as a separate commit from general Unicode lexing unless a tiny lexer
change must be shared.

Validation:

- UnicodeSyntax disabled rejects these forms or treats them as ordinary
  operators as appropriate.
- UnicodeSyntax enabled accepts the reserved forms.
- Arrow-specific forms are still gated on the arrows extension.
- `★` remains gated consistently with `StarIsType`.

## Stage-by-Stage Commit Plan

Completed:

1. `parser: add partial ASCII lexer regression tests`
2. `haskell: widen character literals to code points`
3. `TODO: refresh Unicode implementation plan`
4. `parser: parse numeric character escapes`
5. `haskell: print string literals safely`
6. `haskell: test literal escape semantics`

Next recommended commits:

1. `runtime: widen character constants`
2. `builtins: update Char and Text Unicode behavior`
3. `haskell: add remaining ASCII lexer regression tests`
4. `haskell: classify identifiers by UTF-8 code point`
5. `parser: add Unicode identifier and operator tests`
6. `parser: verify or fix Unicode source locations`
7. `parser: use Unicode classes in Haskell lexer`
8. `parser: parse raw Unicode char and string literals`
9. `parser: finish UnicodeSyntax lexer support`

## Validation Matrix

Each commit should run the smallest useful set of tests for its risk.  Broader
tests are still needed before merging a larger Unicode series.

Always run for parser/lexer changes:

- build `src/bali-phy/bali-phy`
- focused parser/lexer tests touched by the change

Run for literal and identifier changes:

- ASCII lexer literal tests
- ASCII lexer operator/comment tests
- relevant Unicode literal or identifier tests
- `haskell/Types/1`
- `haskell/Read/Containers`

Run before merging a larger series:

- `parse/18`
- `5d +A 50`
- broader Haskell parser tests
- representative generated-Haskell/model tests
