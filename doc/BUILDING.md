# Building documentation

See [README.md](README.md) for source locations and [AUTHORING.md](AUTHORING.md) for writing
conventions. Run the commands below from the checkout root.

## Dependencies and builds

On Ubuntu 24.04, install the system tools:

```sh
sudo apt-get install make python3 python3-pygments weasyprint default-jre-headless nodejs npm fonts-dejavu-core xsltproc docbook-xsl-ns
```

Then set up the user-guide dependencies and build:

```sh
make -C doc setup-user-guide
make -C doc all model-reference
make -C doc check-user-guide
```

Setup obtains xslTNG and MathJax in `doc/_tools/`; it does not manage Python packages or fonts.
Ordinary builds do not install packages. Individual targets are `user-guide`, `tutorial`,
`instructions`, and `model-reference`. The guide also has `UserGuide.html`, `UserGuide.pdf`,
and `UserGuide-A4.pdf` convenience targets.

| Target | Default output directory | Published files |
| --- | --- | --- |
| `user-guide` | `doc/_build/user-guide/` | `UserGuide.html`, `UserGuide.pdf`, `UserGuide-A4.pdf` |
| `tutorial` | `doc/_build/tutorial/` | `Tutorial.html`, `Tutorial.pdf` |
| `instructions` | `doc/_build/instructions/` | `Instructions.html`, `Instructions.pdf` |
| `model-reference` | `doc/_build/model-reference/` | Markdown and YAML metadata for website rendering |

The default PDF paper size is US Letter; `UserGuide-A4.pdf` uses A4. `UserGuide.print.html`
and preprocessed tutorial/exercise XML are intermediate files, not publications.

`make -C doc clean` removes generated outputs, preserving sources and downloaded dependencies.
Individual clean targets are `clean-user-guide`, `clean-tutorial`, `clean-instructions`, and
`clean-model-reference`. Both `_build/` and `_tools/` are ignored by version control.

## Configuration

Override `BUILD_DIR`, `TUTORIAL_BUILD_DIR`, `INSTRUCTIONS_BUILD_DIR`, or `REFERENCE_BUILD_DIR`
to select output directories. Relative paths are interpreted from `doc/`. Use dedicated output
directories containing no handwritten files: the reference clean target removes its whole directory.
`TOOLS_DIR` selects the downloaded dependency directory and defaults to `_tools`.
CI uses explicit directories under the runner's temporary directory.

To use an existing complete xslTNG installation, pass `DOCBOOK_XSLTNG=/path/to/docbook-xslTNG`
to setup and builds. Setup downloads release 2.8.4 by default; this is a known working choice,
not a restriction on other installed releases.

WeasyPrint and Pygments are taken from the normal command path. An optional Python environment
can supply them when distribution packages are unsuitable:

```sh
python3 -m venv doc/_tools/python
. doc/_tools/python/bin/activate
python3 -m pip install -r doc/requirements.txt
```

Install Python venv support and WeasyPrint's native Pango libraries separately. Make respects
the activated environment and never deletes it. Java, Node.js, npm, and fonts are system tools;
the asset checker needs Node.js 18.17 or newer.

MathJax is installed from `package.json` without a project lockfile. Its version range allows
updates within version 3 because the converter uses that package API. To update an existing
installation, run `npm update --prefix doc/_tools/npm --package-lock=false`. After tool updates,
use `make -C doc -B user-guide` to rebuild existing outputs.

## Rendering and maintenance

```text
UserGuide.xml → xslTNG → UserGuide.html → local MathJax in the browser
                             ↓ Node.js MathJax
                         UserGuide.print.html → WeasyPrint → PDFs
```

Browser and PDF rendering share `mathjax-config.js`: delimiters, the base/AMS TeX
packages, and exclusions for code blocks, inline code, and command input. Prose,
terms (including filename patterns), and table entries remain eligible.

The browser MathJax bundle is copied from the same installation used by the PDF
converter. PDF equations are self-contained SVG paths, retaining their dimensions
and baseline offsets. Malformed TeX fails the conversion. The A4 build applies
`user-guide-A4.css` to override only the paper size. WeasyPrint handles pagination,
contents-page numbers, and links. Glyph paths do not guarantee selectable mathematical
text or full PDF accessibility.

Text uses installed fonts, preferring DejaVu serif, sans-serif, and monospace families
where available. The copied xslTNG stylesheet has Google Fonts imports removed, so
viewing and rendering do not require a font download. System font fallbacks are allowed.

The tutorial and Instructions use `add_screen.py` followed by the system DocBook XSL processor
and WeasyPrint. `guide.css` supplies their common styles; `tutorial.css` additionally controls
the tutorial's typography and wrapping. Its `--tutorial-width` sets the browser reading width.

### Contents navigation

The opening HTML contents list shows two section levels. The button at the upper right
opens xslTNG's full contents panel, including a search of section titles. Its checkbox keeps
it open when following links. The panel is embedded in the page and its scripts/styles are
copied locally, so it works from `file:` URLs as well as a web server.

The panel controls work with Enter and Space; Escape closes it. Opening focuses search,
and closing returns focus to the opener when focus was inside the panel. Without JavaScript,
the opening contents list and ordinary links remain usable.

The PDF shows sections and immediate subsections in its opening contents. Each top-level
section starts on a new page and lists its immediate subsections under “In this section”,
with page references; sections without subsections have no local list. Deeper headings remain
in PDF bookmarks. Printed pages have continuous page numbers and a running section heading.
The PDF omits the interactive controls.

`user-guide.xsl` enables the upstream panel; `user-guide.css` limits only the opening screen
list and constrains the panel to the viewport. The XSL also generates local contents from
subsection headings, reusing the upstream ID and heading renderers. These lists are hidden
on screen. `guide-toc.js` adds keyboard controls and hidden-panel focus handling to the upstream script, without implementing search or navigation.
When updating xslTNG, check these interactions in a browser, including a narrow viewport and
local-file viewing. Remove the adapter if upstream supplies equivalent accessibility support.

### Shell selectors

Shell alternatives use xslTNG's native HTML radio buttons and work without JavaScript. Print
shows every alternative. `user-guide.css` makes the hidden radio buttons keyboard-focusable
and provides a visible focus outline. Check Tab and arrow-key navigation, narrow screens,
local-file viewing, and print output when changing panels or upgrading xslTNG. Remove the
keyboard workaround when upstream provides equivalent support. Do not add ARIA tab roles
without implementing their keyboard model.

### Print presentation

In print, the FAQ uses the section's topic list instead of separate question indexes.
Questions appear in bold above their answers, without question numbers or an empty label column.
Long answers can continue onto the next page.

Print styles also use light table stripes, padded cells, compact table paragraphs and
definition lists, and regular-weight commands. Technical text is not automatically hyphenated.
These rules are confined to print media; browser navigation and styling are unchanged.

## Validation

`check-user-guide` builds HTML/PDF, runs the converter tests, and checks internal links
and local assets using MathJax's existing HTML parser. The separate documentation CI
job uses distribution packages and retains the HTML package, PDF, and build logs.

The requirement is faithful, readable output. Inspect equations, missing characters,
tables, code wrapping, contents-page references, and any substantial increase in length.
Small changes in line breaks, pagination, and typography are acceptable. There are no
PDF snapshots or fixed page-count/font-inventory tests.

## Publishing

Copy the user-guide HTML together with `user-guide.css`, `user-guide-assets/` (including
licenses), and `guide-images/`. Copy tutorial HTML with `guide.css` and `tutorial.css`, and
Instructions HTML with `guide.css`. PDFs are standalone. Preserve these relative paths when
serving a document through an alias at another directory level.

The website stores the current guide and tutorial in `doc/4.3/`. Copy the built files and
assets there before changing links. Root `UserGuide.html`, `UserGuide.pdf`, `UserGuide-A4.pdf`,
`Tutorial.html`, and `Tutorial.pdf` are symlinks to those publications. Root asset aliases must
resolve the stylesheets, images, and scripts at the same level. Older current-guide `README.*`
URLs remain compatibility symlinks; keep these until old incoming links no longer need support.
Leave historical release directories intact.

Validate links and PHP in an isolated website checkout before publishing. Install files and
aliases before switching incoming links so that every newly advertised URL already exists.

## Function reference

`make -C doc model-reference` needs only Python 3 and reads the JSON files under `bindings/`;
it does not require a compiled BAli-Phy executable. The generator reports missing descriptions
and unresolved references. Each binding has a directory URL derived from its source path,
with category and alphabetical indexes. Generated Markdown can be rendered with Pandoc or
incorporated into another publishing system.

The website's `python3 update-reference.py /path/to/bali-phy` renders these pages with its PHP
template and sidebar. It requires Pandoc and PHP CLI, checks PHP syntax and internal links,
and replaces `reference/` only after those checks pass. Keep website styles, scripts, and
templates outside that generated directory.
