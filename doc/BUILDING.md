# Building the documentation

The User Guide source is `README.xml`. From this directory, run:

```sh
npm ci
make README.html README.pdf
```

`npm ci` installs the locked build-time dependencies. It is a separate setup step;
`make` does not install packages or download MathJax. Node.js 18 or newer is required
for the converter tests. The remaining tools are Java, Python 3,
Pygments (`pygmentize`), WeasyPrint, and the complete DocBook xslTNG 2.8.4 release.
The build defaults to `~/Applications/docbook-xslTNG-2.8.4`; override it with:

```sh
make DOCBOOK_XSLTNG=/path/to/docbook-xslTNG-2.8.4 README.html README.pdf
```

## HTML and PDF mathematics

The build has these stages:

```text
README.xml → xslTNG → README.html
                                                    ↓ render-math-svg.cjs
                                                README.print.html
                                                    ↓ WeasyPrint
                                                README.pdf
```

The published HTML retains delimited TeX and loads the locally copied MathJax bundle.
Its web fonts are still fetched by the browser. Publish `README.html`
together with `user-guide.css` and `user-guide-assets/`.

For the PDF, MathJax runs locally under Node.js and replaces each delimited TeX expression
with a self-contained SVG. Its glyphs are paths, with no external math-font files
or shared glyph cache. The converter retains equation dimensions and vertical
alignment so inline mathematics sits on the surrounding text's baseline.
Malformed equations fail the conversion instead of silently becoming error glyphs.
MathJax 3.2.2 is pinned to match the browser renderer; updating
that dependency should include visual checks of the PDF equations.

`README.print.html` is generated for WeasyPrint, not for publication. It stays beside
`README.html` so stylesheet and asset paths resolve the same way. WeasyPrint continues
to handle pagination, contents-page numbers, and links. The Tutorial and Instructions
PDF targets retain their existing build rules.

SVG paths preserve the appearance of mathematics, but they do not guarantee
selectable mathematical text or accessible equation descriptions in the PDF.
Existing mathematical labels are retained where MathJax includes them; this is
not a claim of full PDF accessibility.

## Checks and cleanup

```sh
npm test
make README.pdf
```

The focused converter checks cover surrounding links and text, inline placement,
display equations, self-contained SVG output, and malformed mathematics. When
changing the renderer, also inspect equations in the complete PDF, including
`O(L²)`, `Γ₄ + Inv`, variables inside filenames, and model-table entries. Confirm
that the contents list still has page numbers.

`make clean` removes generated HTML (including `README.print.html`), XML, PDFs,
and copied stylesheet assets. It leaves the npm dependencies installed.

## Authoring mathematics

Use `\(…\)` for inline mathematics and `\[…\]` for display mathematics, with TeX
inside the delimiters. Keep each expression in one text segment, without XML markup
inside it. Dollar signs are ordinary text, including in shell examples. MathJax skips
code blocks, inline code, and command input; prose, terms, and table entries remain
eligible. Browser and PDF rendering share `mathjax-config.js`.
