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
- Desugaring can lower non-byte `Hs::Char` values to Core character constants.
- `Core::Constant`, `Runtime::Char`, `Runtime::Exp::as_char()`, character
  closure constructors, and runtime atom/Core conversions use `char32_t`.
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
  the current byte-preserving string guard.
- `util/utf8.{H,cc}` provides shared UTF-8 scalar validation, scalar
  encode/decode, code-point counting, and code-point-to-byte offset conversion.
- Haskell literal printing and lexer numeric escapes use the shared scalar
  validation helper.
- Scalar `Char` conversions such as `ord`, `chr`, `intToChar`, `charToInt`,
  `integerToChar`, and `charToInteger` use Unicode scalar semantics.
- `hPutChar`, `hGetChar`, and `hLookAhead` encode or decode UTF-8 scalars.
- C++ `Text` packing and construction helpers encode `Char` values as UTF-8.
- `Data.Text` stores validated UTF-8 byte slices internally.  Implemented
  Char-facing operations such as `pack`, `unpack`, `singleton`, `cons`,
  `snoc`, `uncons`, `head`, `last`, `tail`, `init`, and `length` use
  code-point semantics, while `append`, `Eq`, `Ord`, `concat`, `fromCppString`,
  `toCppString`, and `Data.Text.IO` preserve validated UTF-8 byte slices.
- `Data.Word.Word8` is an abstract Haskell type with modulo-256 arithmetic,
  represented by an `Int` runtime value for now.
- `Data.ByteString` is a raw byte type backed by immutable shared
  `std::vector<std::byte>` storage with cheap slicing.

Still limited:

- The lexer accepts only ASCII character and string literal source today.
- `Hs::String` and the string-literal parser still store bytes and reject
  numeric escapes above `0xff`.
- `Data.Char` predicates and case transforms are still ASCII-only.
- `CPPString` remains an opaque C++ `std::string` transport, not a text type.
  Generic `Foreign.String` unpacking still maps raw bytes to `Char` values.
- `Data.Text.Encoding` is not implemented yet; it should become the explicit
  bridge between validated `Text` and raw `ByteString`.
- CSV separators are still restricted to one byte.
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
- Temporary byte/string boundaries must be marked with compatibility notes and
  must reject unrepresentable values.

## Design Decisions

- Keep `Hs::Char` as `char32_t`.
- Keep `Hs::String` as `std::string` containing UTF-8 bytes.
- Treat `CPPString` as opaque C++ `std::string` transport.  Wrapper modules
  decide whether a value is validated text, raw bytes, a path, or a diagnostic.
- Treat `Data.Text.Text` as validated UTF-8 over `CPPString`, with byte
  offset/length internally and code-point semantics in the public API.
- Implement `Data.Text.Encoding` on top of the now-separate `Data.ByteString`
  raw-byte representation.
- Add a narrow shared `util/utf8.{H,cc}` module for UTF-8 decoding, UTF-8
  encoding, Unicode scalar validation, and code-point/byte offset conversion.
  Do not let it grow into a general Unicode category or normalization library.
- Use RE/flex Unicode tables for Unicode category checks unless a concrete need
  justifies ICU, libunistring, or another Unicode dependency.
- Keep Unicode category helpers private to `haskell/ids.cc` unless another
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
- In string literals, keep rejecting numeric escapes above `0xff` while
  `Hs::String` remains byte-preserving storage.
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
- Non-byte character literals can desugar to Core and Runtime `Char` values.

Remaining work:

- Optional direct C++ tests for constructed non-ASCII and invalid `Hs::Char`
  printing if a convenient local test hook is added.
- Direct non-ASCII source characters in character literals still need lexer
  Unicode input support.

## Runtime Char and Text Follow-Through

Unicode source support and Unicode runtime character support are related but
not identical.  The current source AST can represent wider characters, but the
runtime cannot.

The runtime transition should push the UTF-8-aware region outward in small
steps.  Each remaining byte-sized boundary must be visible while the transition
is incomplete.

### Shared UTF-8 Helper

Adding `util/utf8.{H,cc}` is shared infrastructure.  The benefit outweighs the
cost because runtime `Char`, `Data.Text`, text IO, source literal printing, and
later identifier validation all need the same UTF-8 and Unicode scalar rules.
Duplicating small decoders in each caller would make it easy for these layers to
accept different malformed inputs or count text differently.

The interface should stay deliberately narrow:

- `bool is_scalar_value(char32_t)`
- decode one UTF-8 scalar from `std::string_view` and a byte offset
- encode one Unicode scalar to UTF-8
- count code points in a validated UTF-8 string
- convert a code-point offset to a byte offset

The helper should not perform normalization, display-width calculation, Unicode
category lookup, case conversion, or grapheme-cluster segmentation.  Those are
separate design decisions.

The first commit that adds this helper should also use it immediately in at
least one real boundary.  Do not leave it as unused parallel infrastructure.

### Temporary Boundary Marker

Any remaining `char32_t` to `char` conversion must be marked with this comment
shape:

```c++
// FIXME-UNICODE: Temporary byte boundary. This rejects non-byte Char values
// until this caller is converted to UTF-8 or char32_t semantics.
```

The marker should be used only where a byte boundary is intentionally retained
during the transition.  Such code must reject unrepresentable values rather than
truncate them.  The marker should make this audit useful:

```sh
rg 'FIXME-UNICODE|static_cast<char>|as_char'
```

As the UTF-8-aware region expands, the number of marked boundaries should shrink.

### Implementation Order

1. Done: `util/utf8.{H,cc}` is wired into `src/util/meson.build` with scalar
   validation, scalar encode/decode, code-point counting, and code-point offset
   to byte offset conversion.
2. Done for the initial scalar-validation sites: Haskell literal printing and
   lexer numeric escapes use `util/utf8`.  String-literal byte storage remains
   marked as a temporary boundary with `FIXME-UNICODE`.
3. Done: widen `Core::Constant` character storage from `char` to `char32_t`.
   Construct character constants explicitly as `char32_t` so the variant is not
   confused with `int`.
4. Done: widen `Runtime::Char` and `Runtime::Exp::as_char()` to `char32_t`.
   Update closure constructors, runtime serialization tests, and conversions
   between Core constants and runtime atoms.
5. Done: update scalar `Char` primitive and builtin functions:
   `ord`, `chr`, `intToChar`, `charToInt`, `integerToChar`,
   `charToInteger`, comparisons, and any remaining arithmetic or conversion
   operations.  Reject invalid Unicode scalar results.
6. Done for now: `Data.Char` predicates and transforms currently implemented
   with `<cctype>` are explicitly ASCII-only with `FIXME-UNICODE` notes.
   Full Unicode behavior is deferred until Unicode category tables are
   designed.
7. Done for the implemented public `Data.Text` surface: internally store UTF-8
   bytes, but expose Unicode code-point semantics for lengths and Char-facing
   operations.  Future operations such as `index`, `take`, `drop`, and
   `splitAt` should use the same code-point offset policy when implemented.
8. Done for `Data.Text`: C++ `Text.pack`, `singleton`, `cons`, and `snoc`
   encode `Char` values as UTF-8, and `Data.Text.unpack`, `head`, `last`,
   `tail`, and `init` decode UTF-8 at the boundary.  Generic
   `Foreign.String` substring unpacking remains raw/transitional.
9. Done for character IO: `hPutChar` encodes one scalar as UTF-8, and
   `hGetChar`/`hLookAhead` decode one UTF-8 scalar.  Raw byte/string APIs may
   remain byte-oriented, but their names or local comments should make that
   explicit.
10. Partly done: byte-oriented consumers are either converted or marked where
    found.  `Data.ByteString` is now a raw byte type.  CSV separators and
    generic `CPPString` unpacking remain marked temporary byte/text boundaries.
11. Done: remove the desugar-time runtime `Char` byte guard.  `Hs::Char` can
    now lower to Core and Runtime `Char` values for all valid Unicode scalars.

### Known Consumer Status

Resolved in the runtime `Char` widening:

- `Core::Constant`
- `Runtime::Char`
- `Runtime::Exp::as_char()`
- closure constructors for character values
- `runtime_exp_from_core_constant()`
- `core_constant_from_runtime_exp()`
- `atom_from_constant()`
- `runtime_constant_to_core()`
- `hPutChar`
- `hGetChar`
- `hLookAhead`
- `ord`, `chr`, `intToChar`, `charToInt`, `integerToChar`, and
  `charToInteger`
- `Runtime::RVector` character vectors, now using `std::vector<char32_t>`
- `Data.Text` construction, unpacking, basic Char-facing operations, length,
  equality, ordering, concatenation, `toCppString`, and `Data.Text.IO`
  read/write paths

Remaining or intentionally limited:

- `Data.Char` predicates and case transforms are ASCII-only until Unicode
  category and case-conversion support is designed.
- Future `Data.Text` indexing and slicing operations such as `index`, `take`,
  `drop`, and `splitAt` should use code-point offsets when they are added.
- `Foreign.String` substring unpacking still maps raw bytes to `Char` values
  through `CPPString`; `Data.Text.unpack` no longer uses that path.
- `Data.Text.Encoding` remains deferred, but `Data.ByteString` now provides the
  raw-byte representation it should use.
- `hPutStrRaw`, `hGetLineRaw`, `hGetContentsRaw`, and other raw string APIs
  remain byte-oriented by design, but callers that expose `[Char]` need UTF-8
  decoding.
- CSV separator handling in `Data.cc` is still restricted to one byte.
- String literals and `Hs::String` remain byte-preserving.

### Validation

Add tests as each layer is converted rather than waiting until the end:

- Runtime non-ASCII `Char` construction and printing where applicable.
- `ord` and `chr` above `255`.
- Rejection of surrogate and out-of-range scalar values.
- `Text.singleton`, `Text.pack`, and `Text.unpack` with multibyte code points.
- `Text.length` counts code points, not UTF-8 bytes.
- Future `Text.index`, `take`, `drop`, and `splitAt` tests should confirm they
  operate on code-point offsets when those functions are implemented.
- `hPutChar` and `hGetChar` round-trip UTF-8.
- Existing byte-boundary rejection tests are removed or updated as their
  corresponding temporary boundaries disappear.

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
