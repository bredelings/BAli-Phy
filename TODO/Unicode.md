# Unicode Support Plan

## Goal

Finish Unicode support for BAli-Phy's Haskell implementation: UTF-8 source
input, Unicode identifiers and operators, raw non-ASCII character and string
literals, Unicode-aware `Data.Char`, and clear handling of the few remaining
byte-oriented compatibility boundaries.

This document focuses on remaining work.  It intentionally omits historical
lists of completed work.

## Remaining Scope

- Haskell source lexing still uses placeholder Unicode classes in
  `src/computation/parser/lexer.l`.
- `src/computation/haskell/ids.cc` still validates identifiers byte-by-byte
  with ASCII rules.
- `Data.Char` predicates and case conversions are ASCII-only, and `isAscii`
  / `isLatin1` currently do not inspect their argument.
- Raw non-ASCII source characters in character and string literals are not yet
  accepted, even though numeric escapes and runtime `Char` already support
  Unicode scalar values.
- `UnicodeSyntax` reserved symbols are documented in comments but are not
  installed in the active reserved-symbol tables.
- CSV splitting still accepts only a one-byte separator.
- `Foreign.String.getStringElement` and `unpack_cpp_substring` remain raw-byte
  compatibility helpers.  Text callers should continue to use
  `unpackUtf8String`.
- `Data.Text` and `Data.Text.Encoding` have a small implemented surface.
  Future operations must preserve byte-slice storage internally while exposing
  code-point semantics in their public APIs.
- `src/models/lexer.l` has similar placeholder Unicode classes.  That model
  lexer is outside the Haskell-source plan below unless we deliberately decide
  to make the model language source Unicode-aware too.

## Invariants

- Source files are UTF-8.
- Invalid UTF-8 is rejected at the first boundary that interprets bytes as
  Unicode.
- `Char` values are Unicode scalar values, excluding surrogates and values
  above `0x10ffff`.
- Identifier equality is exact UTF-8 byte equality after validation.  Do not
  normalize identifiers.
- Parser columns and layout indentation count Unicode code points, not UTF-8
  bytes.  Grapheme clusters and display width remain out of scope.
- Numeric Haskell literals remain ASCII-only.  Unicode decimal digits may occur
  in identifiers, but not in numeric literal syntax.

## Shared Unicode Category Helper

### Design Analysis

Some new infrastructure is justified here.  `ids.cc`, `Char.cc`, and lexer
tests all need the same Unicode category decisions.  Duplicating direct
`reflex::Unicode::range(...)` table walks would make those decisions drift.

The helper should be narrow and table-oriented:

- `bool in_category(char32_t c, std::string_view category)`
- `bool in_any_category(char32_t c, std::initializer_list<std::string_view>)`
- `char32_t to_upper(char32_t c)`
- `char32_t to_lower(char32_t c)`

Implement it in a small C++ module such as `src/util/unicode-categories.{H,cc}`
backed by `reflex/unicode.h`.  Do not add normalization, grapheme segmentation,
display-width logic, or a full Haskell `GeneralCategory` type in this step.
That keeps the cost proportional to the current need and gives both identifier
classification and `Data.Char` one shared source of truth.

## Implementation Plan

### 1. Pin Current ASCII Lexer Behavior

Before enabling Unicode lexer mode, add focused tests for behavior that must
not regress:

- ASCII identifiers and qualified identifiers.
- ASCII symbolic operators.
- `--` comments versus operators such as `--+` and `-->`.
- Layout after ASCII identifiers and keywords.
- Existing character and string literal escape behavior.

Run the focused tests before and after each lexer commit.

### 2. Add Unicode Category Support

Add `src/util/unicode-categories.{H,cc}` and wire it into Meson.  Implement
range membership with `reflex::Unicode::range(name)`, whose ranges are
`[lo, hi]` integer pairs terminated by `0, 0`.

Add small C++ or Haskell-visible tests for representative categories:

- `Lu`: `A`, `Α`
- `Ll`: `a`, `α`
- `Lt`: a titlecase letter
- `Lo`: a letter without case
- `Nd`: a non-ASCII decimal digit
- `Sm` or `So`: a Unicode symbol operator candidate

### 3. Convert `Data.Char`

Update `src/builtins/Char.cc` to remove the temporary ASCII guard from
predicates and case conversion.

Required behavior:

- `isAscii c = ord c <= 0x7f`
- `isLatin1 c = ord c <= 0xff`
- `isControl`: Unicode `Cc`
- `isLower`: `Ll`
- `isUpper`: `Lu`
- `isAlpha`: `L`
- `isAlphaNum`: `L` or `N`
- `isDigit`: `Nd`
- `isHexDigit`: ASCII `0-9`, `a-f`, `A-F` unless we deliberately choose a
  broader Haskell-compatible definition later.
- `isSpace`: Unicode space class used by RE/flex plus existing ASCII whitespace
  behavior.
- `isPrint`: not control, surrogate, or invalid; refine only if tests show a
  mismatch with expected Haskell behavior.
- `isPunctuation`: `P`
- `toUpper` / `toLower`: `reflex::Unicode::toupper/tolower`

Add tests for non-ASCII letters, digits, spaces, and case conversion.  Keep
`chr` and `ord` scalar-value tests.

### 4. Convert Identifier Validation

Rewrite `src/computation/haskell/ids.cc` to iterate over UTF-8 code points with
`util/utf8::decode_next`, while retaining byte offsets for substrings.

Classification rules:

- `large`: ASCII `A-Z`, Unicode `Lu`, `Lt`
- `small`: ASCII `a-z`, `_`, Unicode `Ll`, `Lo`
- identifier continuation: large, small, ASCII digits, Unicode `Nd`, `Nl`,
  `No`, `Lm`, `Mn`, and `'`
- symbols: ASCII symbol chars plus Unicode `Pc`, `Pd`, `Po`, `Sm`, `Sc`,
  `Sk`, `So`
- `:` remains the only constructor-symbol starter.
- Unicode decimal digits are identifier continuation characters, not numeric
  literal digits.

Update the path-splitting helpers at the same time:

- `skip_conid_dot`
- `find_module_separator`
- `haskell_name_path`
- `get_haskell_identifier_path`

Validation:

- Unicode varids, conids, symbols, and qualified names.
- Invalid UTF-8 rejected.
- Existing ASCII identifier tests still pass.

### 5. Enable Unicode in the Haskell Lexer

First checkpoint: verify RE/flex column behavior with `%option unicode`.
If `matcher().columno()` remains byte-based, update `yylloc_update` to compute
code-point columns from the matched UTF-8 text.  If it is already code-point
based, add tests that pin that behavior.

Then update `src/computation/parser/lexer.l`:

- Add `%option unicode`.
- Replace placeholder classes:
  - `unilarge`: `\p{Lu}` / `\p{Lt}`
  - `unismall`: `\p{Ll}` / `\p{Lo}`
  - `unidigit`: `\p{Nd}` for identifier continuation only
  - `unisymbol`: `\p{Pc}`, `\p{Pd}`, `\p{Po}`, `\p{Sm}`, `\p{Sc}`,
    `\p{Sk}`, `\p{So}`
  - `unispace`: the Unicode space class selected for `Data.Char.isSpace`
  - `unigraphic`: valid non-control, non-separator source characters allowed
    in char/string literals
  - `uniidchar`: `\p{Nd}`, `\p{Nl}`, `\p{No}`, `\p{Lm}`, `\p{Mn}`
- Keep decimal, binary, octal, hexadecimal, and exponent syntax ASCII-only.
- Extend varsym lookahead so Unicode opening lexemes are treated like ASCII
  opening lexemes for prefix/tight-infix classification.
- Regenerate `lexer.cc` with `src/computation/parser/gen_parser.sh`.

Validation:

- Unicode identifiers and qualified names parse.
- Unicode symbolic operators parse.
- `--` followed by a Unicode symbol is an operator, not a comment.
- Layout after Unicode identifiers uses code-point columns.
- ASCII lexer tests still pass.

### 6. Parse Raw Unicode Character and String Literals

Update literal parsing helpers in `lexer.l` so raw non-ASCII literal payloads
are decoded with `util/utf8::decode_next` instead of treated as individual
bytes.

Required behavior:

- A raw non-ASCII character literal produces one `char32_t` scalar.
- A raw non-ASCII string literal stores valid UTF-8 bytes in `Hs::String`.
- Escape handling remains ASCII and keeps existing numeric escape semantics.
- Malformed UTF-8 inside a literal reports a lexer/parser diagnostic.

Validation:

- Raw non-ASCII character literal.
- Raw non-ASCII string literal.
- Mixed raw and escaped string literal.
- Malformed UTF-8 in character and string literals.
- Existing numeric escape tests still pass.

### 7. Finish `UnicodeSyntax`

Move the UnicodeSyntax mappings from comments in `driver.cc` into the active
reserved-symbol setup, gated by the existing language extensions.

Mappings:

- `∷` -> `::`
- `⇒` -> `=>`
- `∀` -> `forall`
- `→` -> `->`
- `←` -> `<-`
- `⤙` -> `-<` when arrows are enabled
- `⤚` -> `>-` when arrows are enabled
- `⤛` -> `-<<` when arrows are enabled
- `⤜` -> `>>-` when arrows are enabled
- `★` -> `*` when `UnicodeSyntax` and `StarIsType` allow it

Validation:

- UnicodeSyntax disabled rejects or treats these as ordinary operators as
  intended.
- UnicodeSyntax enabled accepts the reserved forms.
- Arrow-specific forms remain gated on arrows.
- `★` remains consistent with `StarIsType`.

### 8. Clean Up Remaining Byte Boundaries

Handle these after source Unicode and `Data.Char` are stable:

- Keep every intentional temporary boundary marked with `FIXME-UNICODE` or a
  compatibility note explaining whether it treats a `CPPString` as bytes,
  validated UTF-8 text, a path, or a diagnostic string.
- CSV: replace one-byte separator handling in `src/builtins/Data.cc` with a
  UTF-8 separator string and split on `std::string`, or keep the byte-only
  restriction with an explicit public API note.
- `Foreign.String`: keep `getStringElement` and `unpack_cpp_substring` marked
  as raw-byte compatibility helpers until no Haskell caller needs them for
  text.
- `Data.Text`: when adding `index`, `take`, `drop`, `splitAt`, `span`, `break`,
  `lines`, `words`, and similar APIs, use public code-point offsets and keep
  internal byte offsets.
- `Data.Text.Encoding`: add `decodeUtf8'`, lenient decoding, UTF-16/32 codecs,
  and streaming APIs only when a caller needs them; each should be backed by
  `ByteString`, not raw `[Char]`.

## Validation Before Merging

Run:

- `ninja -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O`
- focused lexer/parser tests touched by the change
- Unicode identifier/operator/literal tests
- `haskell/Data/Text/Unicode`
- `haskell/Data/Text/Encoding`
- `haskell/IO/UnicodeChar`
- `haskell/String/UnicodeEscapes`
- `parse/18`
- `bali-phy:bali-phy 5d +A 50`

Coverage check against `TODO/Unicode.md`: this plan keeps the old remaining
topics for identifier classification, source locations/layout, Unicode lexer
classes, operators/comments, raw literals, UnicodeSyntax, `Data.Char`, CSV,
`Data.Text.Encoding`, byte-boundary markers, and final generated-Haskell/model
validation.  Completed-history sections from the old document were deliberately
not copied.
