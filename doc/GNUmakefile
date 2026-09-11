all: README.html README.pdf Tutorial.html Tutorial.pdf Instructions.html Instructions.pdf

# Build the User Guide with `make README.html README.pdf`. Requires Java, Python 3,
# pygmentize (Pygments), Node.js/npm, WeasyPrint, and the xslTNG release distribution
# (including its libraries).
# Override its location with `make DOCBOOK_XSLTNG=/path/to/docbook-xslTNG-2.8.4 ...`.
# Run `npm ci` in this directory once to install the pinned PDF math renderer.
# Browsers download the default web fonts and MathJax; PDF math is rendered locally to SVG.
DOCBOOK_XSLTNG ?= $(HOME)/Applications/docbook-xslTNG-2.8.4
GUIDE_ASSETS = $(addprefix user-guide-assets/,docbook.css docbook-paged.css draft-left.svg draft-right.svg LICENSE)
MATH_ASSETS = user-guide-assets/mathjax-config.js user-guide-assets/tex-svg.js user-guide-assets/MathJax-LICENSE

.DELETE_ON_ERROR:

README.html: README.xml user-guide.xsl $(GUIDE_ASSETS) $(MATH_ASSETS) $(DOCBOOK_XSLTNG)/bin/docbook \
             $(DOCBOOK_XSLTNG)/xslt/catalog.xml $(DOCBOOK_XSLTNG)/xslt/docbook.xsl
	@command -v pygmentize >/dev/null || { echo 'User Guide requires pygmentize (Pygments).' >&2; exit 1; }
	"$(DOCBOOK_XSLTNG)/bin/docbook" -catalog:"$(DOCBOOK_XSLTNG)/xslt/catalog.xml" \
	    -xi:on -s:README.xml -xsl:user-guide.xsl -o:$@

user-guide-assets/%: $(DOCBOOK_XSLTNG)/resources/css/%
	mkdir -p user-guide-assets
	cp "$<" "$@"

user-guide-assets/LICENSE: $(DOCBOOK_XSLTNG)/LICENSE
	mkdir -p user-guide-assets
	cp "$<" "$@"

README.print.html: README.html render-math-svg.cjs mathjax-config.js package.json package-lock.json
	@test -f node_modules/mathjax-full/package.json || { echo 'Run npm ci in doc/ before building the PDF.' >&2; exit 1; }
	node render-math-svg.cjs $< $@

README.pdf: README.print.html user-guide.css $(GUIDE_ASSETS)
	weasyprint $< $@
Tutorial.pdf Instructions.pdf: guide.css

user-guide-assets/mathjax-config.js: mathjax-config.js
	mkdir -p user-guide-assets
	cp "$<" "$@"

user-guide-assets/tex-svg.js: node_modules/mathjax-full/es5/tex-svg.js
	mkdir -p user-guide-assets
	cp "$<" "$@"

user-guide-assets/MathJax-LICENSE: node_modules/mathjax-full/LICENSE
	mkdir -p user-guide-assets
	cp "$<" "$@"

clean:
	-@rm -f *.html *.xhtml *.fo *.pdf *~
	-@rm -f $(GUIDE_ASSETS) $(MATH_ASSETS)


%.xml: %.tut.xml
	./add_screen.py < $^ > $@

%.html: %.xml docbook-html.xsl
	xsltproc --nonet --xinclude docbook-html.xsl $*.xml > $@

%.html: %.md
	pandoc -s $*.md --css pandoc.css --toc > $@

%.pdf: %.html
	weasyprint $*.html $*.pdf
