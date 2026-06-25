# Unicode Support Plan

## Goal

Add Unicode support to BAli-Phy's Haskell implementation in stages.  The
target is UTF-8 Haskell source input, Unicode scalar `Char` literals, Unicode
identifiers and operators, and no identifier normalization initially.

Identifier equality should remain exact UTF-8 byte-sequence equality after
validation.

## Design Decisions

- Store `Hs::Char` as a Unicode scalar value using `char32_t`.
- Keep `Hs::String` as UTF-8 `std::string`.
- Treat one Unicode code point as one source column for parser and layout
  purposes.
- Reuse RE/flex Unicode tables for Unicode category checks.  Do not add
  ICU/libunistring unless later requirements justify the extra dependency.
- Keep UTF-8/category helpers private to `haskell/ids.cc` unless another
  concrete caller appears.
- Add `UnicodeSyntax` separately from basic Unicode identifier and operator
  support.
- Do not normalize identifiers at first.  This keeps source names predictable
  and avoids introducing normalization tables or policy before they are needed.

## Baseline Tests

Add tests that lock down current ASCII behavior before changing
representation:

- ASCII char and string literals.
- Escaped char and string literals.
- ASCII identifiers and qualified identifiers.
- ASCII symbolic operators.
- `--` comment behavior versus `--+` or `-->` operator behavior.
- Layout with ASCII identifiers.

These tests should make representation and scanner changes easier to review by
showing that ordinary ASCII Haskell behavior is unchanged.

## Character Representation

Update all Haskell char literal storage and parser token types:

- Change `Hs::Char::value` from `char` to `char32_t`.
- Change `Literal::is_Char()` to return `std::optional<char32_t>`.
- Change parser tokens `CHAR` and `PRIMCHAR` to carry `char32_t`.
- Update lexer helper `make_char()` to return `char32_t`.
- Update desugar, typecheck, and pattern call sites.

Keep the lexer accepting only ASCII char literals at this step.  This prevents
accepting Unicode source before the representation and printing semantics are
correct.

## Runtime Char and Text Follow-Through

`Hs::Char` can be widened before the runtime is fully Unicode-aware, but that
does not complete Unicode character support.

Current follow-up work:

- `Core::Constant` still stores character constants as byte-sized `char`.
- `Runtime::Char` still stores byte-sized `char`.
- `Runtime::Exp::as_char()` and related closure constructors still assume
  byte-sized characters.
- `Data.Char` builtins should eventually use Unicode scalar values and Unicode
  category tables instead of ASCII/byte predicates.
- `Data.Text` and associated builtins should eventually treat text as valid
  UTF-8, with explicit encode/decode boundaries for `[Char]`.
- `Text.pack`, `Text.unpack`, `hPutChar`, `hGetChar`, `intToChar`,
  `charToInt`, and `integerToChar` need Unicode-scalar/UTF-8 semantics.

Until that work is done, desugaring should reject any `Hs::Char` value that
cannot be represented by the current byte-sized runtime `Char`.  This prevents
silent truncation while keeping the source AST ready for later Unicode work.

## Character Literal Semantics

Add Haskell-source escaping for `char32_t`:

- Reject invalid scalar values: surrogates and values above `0x10ffff`.
- Preserve standard escapes: `'\n'`, `'\t'`, `'\r'`, `'\''`, `'\\'`, and
  similar ASCII escapes.
- Print safe printable ASCII directly.
- Print non-ASCII and non-printable code points using numeric escapes.
- Add tests for ASCII, escaped ASCII, non-ASCII code point construction, and
  invalid code points.

## String Literal Printing

Keep `Hs::String` as UTF-8 bytes, but make printing explicit:

- Escape quotes, backslashes, and control characters.
- Decide whether valid non-ASCII UTF-8 prints raw or as numeric escapes.  Raw
  UTF-8 is preferable if the generated Haskell pipeline handles it reliably;
  otherwise use numeric escapes.
- Reject or escape invalid UTF-8 deliberately.  Do not silently emit malformed
  Haskell source.
- Add tests for quotes, backslashes, newline/control chars, and non-ASCII
  UTF-8.

## UTF-8 Iteration in `ids.cc`

Add narrow helper code inside `haskell/ids.cc`:

- Iterate over `std::string_view`.
- Yield `{char32_t codepoint, size_t begin_byte, size_t end_byte}`.
- Treat invalid UTF-8 as invalid identifier input.
- Use byte offsets for substrings, so module qualifier splitting remains valid.
- Use RE/flex `reflex::utf8()` for decoding if suitable.

Design analysis: this helper is infrastructure, but it is justified because
qualifier splitting, identifier validation, and symbol validation all need the
same UTF-8 decoding.  Keeping it private limits its cost.

## Unicode Category Membership

Still private to `ids.cc`, add a helper around `reflex::Unicode::range()`:

- Ranges are `[lo, hi]` integer pairs ending in `0, 0`.
- Add `range_contains("Lu", cp)` or an equivalent helper.
- Cache range pointers or use small static accessors to avoid repeated lookup
  noise.
- Test category membership for representative code points if practical.

## Identifier Classification

Replace byte-based classification with code-point classification:

- large: ASCII `A-Z`, Unicode `Lu`, `Lt`
- small: ASCII `a-z`, `_`, Unicode `Ll`, `Lo`
- identifier continuation: small, large, ASCII digits, Unicode `Nd`, `Nl`,
  `No`, `Lm`, `Mn`, and `'`
- symbol: ASCII symbol chars plus Unicode `Pc`, `Pd`, `Po`, `Sm`, `Sc`, `Sk`,
  `So`

Update these functions:

- `is_haskell_id`
- `is_haskell_varid`
- `is_haskell_conid`
- `is_haskell_uqsym`
- `is_haskell_varsym`
- `is_haskell_consym`
- `skip_conid_dot`
- `find_module_separator`
- `get_haskell_identifier_path`

Identifier comparison should remain exact UTF-8 string comparison.  No
normalization is performed.

## Identifier Tests

Add focused tests or strong indirect coverage for:

- Unicode lowercase varids.
- Unicode uppercase/titlecase conids.
- Unicode symbolic operators.
- Unicode qualified names.
- Invalid UTF-8 rejected.
- Invalid identifier code points rejected.
- Module qualifier splitting with Unicode components.

## Source Locations

Before changing lexer rules, verify RE/flex Unicode location behavior:

- What do `matcher().columno()` and `matcher().columno_end()` report in
  `%option unicode` mode?
- Are columns bytes or code points?
- How are tabs counted relative to current behavior?

If RE/flex counts bytes, add a small location-update helper that computes
code-point columns for matched text.  Do a separate design analysis before
adding that helper.

## Lexer Unicode Categories

Replace placeholder classes in `computation/parser/lexer.l`:

- `unilarge`
- `unismall`
- `unidigit`
- `unisymbol`
- `unispace`
- `unigraphic`
- `uniidchar`

Use RE/flex Unicode property classes matching the `haskell/ids` mapping.  Keep
decimal numeric literals ASCII-only.

## Operators and Comments

Add or verify lexer behavior for:

- `--` followed by a Unicode symbol should be a varsym, not a comment.
- Unicode symbol operators adjacent to identifiers.
- Prefix/suffix/tight-infix classification with Unicode identifier starts after
  an operator.
- `varsym` lookahead must include Unicode opening lexeme cases, not only ASCII
  starts.

## RE/flex Unicode Mode

Add `%option unicode` and regenerate scanner sources using
`computation/parser/gen_parser.sh`.

Ensure generated files are updated as needed:

- `lexer.cc`
- `parser.cc`
- `parser.hh`

`parser.cc` and `parser.hh` will change when token value types change.

## Unicode Char and String Parsing

After lexer Unicode mode and `Hs::Char` support are stable:

- Parse raw non-ASCII char literals as `char32_t`.
- Parse raw non-ASCII string literal content as UTF-8.
- Keep escape behavior explicit and tested.
- Reject malformed UTF-8 with a lexer/parser diagnostic.

## UnicodeSyntax

Add `LangExt::UnicodeSyntax` as a separate stage.  Reserved symbol mappings
should include:

- `∷` -> `::`
- `⇒` -> `=>`
- `∀` -> `forall`
- `→` -> `->`
- `←` -> `<-`
- optionally `★` -> `*` when `StarIsType` applies

Keep this as a separate commit from general Unicode lexing.

## Validation

Run:

- build `src/bali-phy/bali-phy`
- focused parser/lexer tests
- `parse/18`
- `5d +A 50`
- broader Haskell parser tests
- representative generated-Haskell/model tests

## Proposed Commit Order

1. `parser: add ASCII lexer regression tests`
2. `haskell: widen character literals to code points`
3. `haskell: print char and string literals safely`
4. `haskell: classify identifiers by UTF-8 code point`
5. `parser: add Unicode lexer tests`
6. `parser: use Unicode classes in Haskell lexer`
7. `parser: parse Unicode char and string literals`
8. `parser: add UnicodeSyntax tokens`
