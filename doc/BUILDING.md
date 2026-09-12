# Building the User Guide

Install ordinary system tools first. On Ubuntu 24.04:

```sh
sudo apt-get install make python3 python3-pygments weasyprint default-jre-headless nodejs npm fonts-dejavu-core
```

Then, from the checkout root:

```sh
make -C doc setup-user-guide
make -C doc check-user-guide
```

Setup obtains xslTNG if necessary and installs MathJax. It does not manage Python
packages or fonts. Subsequent builds need only `make -C doc user-guide`.
`README.html` and `README.pdf` remain convenience targets.

Outputs are in `../../build/user-guide` relative to `doc/`. Publish `README.html` with
`user-guide.css` and `user-guide-assets/`, including licenses. `README.pdf` is standalone.
`README.print.html` is an intermediate input to WeasyPrint, not a file to publish.
`make -C doc clean-user-guide` removes generated guide files, preserving the source
and installed tools. Tutorial and Instructions retain their older build rules.

## Installation choices

Use `BUILD_DIR` and `TOOLS_DIR` to override the output and tool directories. Tools
default to `../../build/user-guide-tools`, relative to `doc/`. To use an existing
complete xslTNG installation, pass `DOCBOOK_XSLTNG=/path/to/docbook-xslTNG` to both
setup and builds. Setup downloads release 2.8.4 by default; this is a known working
choice, not a restriction on other installed releases.

WeasyPrint and Pygments are taken from the normal command path. If distribution
packages are unsuitable or unavailable, an optional virtual environment can supply them:

```sh
python3 -m venv ../build/guide-python
. ../build/guide-python/bin/activate
python3 -m pip install -r doc/requirements.txt
```

Install Python venv support and WeasyPrint's native Pango libraries if your system does
not already provide them. Make respects the activated environment and never deletes it.
Java, Node.js, npm, and fonts are installed separately with the system's usual tools.
Node.js 18.17 or newer is needed by the asset checker.

MathJax is installed from `package.json` without a project lockfile. Its version range
allows updates within version 3 because the converter currently uses that package API.
Rerun setup to install dependencies on another machine; use `npm update --prefix ../build/user-guide-tools/npm --package-lock=false` from the checkout root to update
an existing default installation. Rebuild with `make -C doc -B user-guide` after tool
updates if generated files are already present.

## Authoring and rendering

The source is ordinary XML in `README.xml`. Use `\(…\)` for inline mathematics and
`\[…\]` for display mathematics. Keep each expression in one text segment, without
XML markup inside it. XML still requires escaping `<` and `&` inside TeX.
Dollar signs are ordinary text, including in shell examples.

Browser and PDF rendering share `mathjax-config.js`: delimiters, the base/AMS TeX
packages, and exclusions for code blocks, inline code, and command input. Prose,
terms (including filename patterns), and table entries remain eligible.

```text
README.xml → xslTNG → README.html → local MathJax in the browser
                         ↓ Node.js MathJax
                     README.print.html → WeasyPrint → README.pdf
```

The browser MathJax bundle is copied from the same installation used by the PDF
converter. PDF equations are self-contained SVG paths, retaining their dimensions
and baseline offsets. Malformed TeX fails the conversion. WeasyPrint handles pagination,
contents-page numbers, and links. Glyph paths do not guarantee selectable mathematical
text or full PDF accessibility.

Text uses installed fonts, preferring DejaVu serif, sans-serif, and monospace families
where available. The copied xslTNG stylesheet has Google Fonts imports removed, so
viewing and rendering do not require a font download. System font fallbacks are allowed.

## Table layout

Set table column proportions with DocBook `colspec` entries in `README.xml`; the same
proportions serve HTML and PDF. Give numeric columns less room than descriptions, but
allow space for expressions in parameter-count columns. Use ordinary spaces between
model-expression components so they can wrap without splitting identifiers.

Print CSS suppresses automatic hyphenation and arbitrary word breaks in table code and
tries to keep each row on one page. Tables may continue across pages with repeated headings;
rows taller than a page must still be allowed to split. After changing tables, inspect the PDF
for overflow, split identifiers, awkward gaps, and readable continuation headings. Check narrow
HTML layouts too, where wide tables scroll. Exact pagination is not a requirement.

## Contents navigation

The opening HTML contents list shows two section levels. The button at the upper right
opens xslTNG's full contents panel, including a search of section titles. Its checkbox keeps
it open when following links. The panel is embedded in the page and its scripts/styles are
copied locally, so it works from `file:` URLs as well as a web server.

The panel controls work with Enter and Space; Escape closes it. Opening focuses search,
and closing returns focus to the opener when focus was inside the panel. Without JavaScript,
the opening contents list and ordinary links remain usable. The PDF retains the full contents
list with page numbers and omits the interactive controls.

`user-guide.xsl` enables the upstream panel; `user-guide.css` limits only the opening screen
list and constrains the panel to the viewport. `guide-toc.js` adds keyboard controls and
hidden-panel focus handling to the upstream script, without implementing search or navigation.
When updating xslTNG, check these interactions in a browser, including a narrow viewport and
local-file viewing. Remove the adapter if upstream supplies equivalent accessibility support.

## Checks and maintenance

`check-user-guide` builds HTML/PDF, runs the converter tests, and checks internal links
and local assets using MathJax's existing HTML parser. The separate documentation CI
job uses distribution packages and retains the HTML package, PDF, and build logs.

The requirement is faithful, readable output. Inspect equations, missing characters,
tables, code wrapping, contents-page references, and any substantial increase in length.
Small changes in line breaks, pagination, and typography are acceptable. There are no
PDF snapshots or fixed page-count/font-inventory tests.

Tool versions used in successful builds are useful diagnostic information, not exact
requirements. Add compatibility restrictions only for demonstrated problems. A clean Ubuntu 24.04 build passed with its packaged WeasyPrint 61.1, Pygments 2.17.2,
and Node.js 18.19.1. Local checks also passed with WeasyPrint 69.0, Pygments 2.20.0,
Java 25, and Node.js 24. These are tested examples, not a required version list.
