all: README.html README.pdf Tutorial.html Tutorial.pdf Instructions.html Instructions.pdf

# Build the User Guide with `make README.html README.pdf`. Requires Java, Python 3,
# itex2MML, pygmentize (Pygments), WeasyPrint, and the xslTNG release distribution
# (including its libraries).
# Override its location with `make DOCBOOK_XSLTNG=/path/to/docbook-xslTNG-2.8.4 ...`.
# Browsers download the default web fonts and MathJax; WeasyPrint does not run MathJax.
DOCBOOK_XSLTNG ?= $(HOME)/Applications/docbook-xslTNG-2.8.4
GUIDE_ASSETS = $(addprefix user-guide-assets/,docbook.css docbook-paged.css draft-left.svg draft-right.svg LICENSE)

.DELETE_ON_ERROR:

README.html: README.xml user-guide.xsl $(GUIDE_ASSETS) $(DOCBOOK_XSLTNG)/bin/docbook \
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

README.pdf: user-guide.css $(GUIDE_ASSETS)
Tutorial.pdf Instructions.pdf: guide.css

clean:
	-@rm -f *.html *.xhtml README.xml *.fo *.pdf *~
	-@rm -f $(GUIDE_ASSETS)


%.xml: %.itex.xml
	itex2MML < $^ > $@

%.xml: %.tut.xml
	./add_screen.py < $^ > $@

%.html: %.xml docbook-html.xsl
	xsltproc --nonet --xinclude docbook-html.xsl $*.xml > $@

%.html: %.md
	pandoc -s $*.md --css pandoc.css --toc > $@

%.pdf: %.html
	weasyprint $*.html $*.pdf
