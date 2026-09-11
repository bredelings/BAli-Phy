all: user-guide Tutorial.html Tutorial.pdf Instructions.html Instructions.pdf

# Run from doc/; BUILD_DIR contains only generated User Guide outputs.
# Dependency setup is explicit; ordinary builds never install packages.
BUILD_DIR ?= ../../build/user-guide
DOCBOOK_XSLTNG ?= $(HOME)/Applications/docbook-xslTNG-2.8.4
ASSET_DIR = $(BUILD_DIR)/user-guide-assets
GUIDE_ASSETS = $(addprefix $(ASSET_DIR)/,docbook.css docbook-paged.css draft-left.svg draft-right.svg LICENSE)
MATH_ASSETS = $(addprefix $(ASSET_DIR)/,mathjax-config.js tex-svg.js MathJax-LICENSE)

.DELETE_ON_ERROR:
.PHONY: all user-guide README.html README.pdf clean clean-user-guide

user-guide: $(BUILD_DIR)/README.html $(BUILD_DIR)/README.pdf
README.html: $(BUILD_DIR)/README.html
README.pdf: $(BUILD_DIR)/README.pdf

$(BUILD_DIR)/README.html: README.xml user-guide.xsl $(GUIDE_ASSETS) $(MATH_ASSETS) $(BUILD_DIR)/user-guide.css \
                        $(DOCBOOK_XSLTNG)/bin/docbook $(DOCBOOK_XSLTNG)/xslt/catalog.xml \
                        $(DOCBOOK_XSLTNG)/xslt/docbook.xsl
	@command -v pygmentize >/dev/null || { echo 'User Guide requires pygmentize (Pygments).' >&2; exit 1; }
	"$(DOCBOOK_XSLTNG)/bin/docbook" -catalog:"$(DOCBOOK_XSLTNG)/xslt/catalog.xml" \
	    -xi:on -s:README.xml -xsl:user-guide.xsl -o:"$@"

$(ASSET_DIR)/%: $(DOCBOOK_XSLTNG)/resources/css/%
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(ASSET_DIR)/LICENSE: $(DOCBOOK_XSLTNG)/LICENSE
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(BUILD_DIR)/user-guide.css: user-guide.css
	mkdir -p "$(BUILD_DIR)"
	cp "$<" "$@"

$(ASSET_DIR)/mathjax-config.js: mathjax-config.js
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(ASSET_DIR)/tex-svg.js: node_modules/mathjax-full/es5/tex-svg.js
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(ASSET_DIR)/MathJax-LICENSE: node_modules/mathjax-full/LICENSE
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(BUILD_DIR)/README.print.html: $(BUILD_DIR)/README.html render-math-svg.cjs mathjax-config.js package.json package-lock.json
	node render-math-svg.cjs "$<" "$@"

$(BUILD_DIR)/README.pdf: $(BUILD_DIR)/README.print.html $(BUILD_DIR)/user-guide.css $(GUIDE_ASSETS)
	weasyprint "$<" "$@"

Tutorial.pdf Instructions.pdf: guide.css

# Name the generated files explicitly: README.xml is now a source file.
clean-user-guide:
	$(RM) "$(BUILD_DIR)/README.html" "$(BUILD_DIR)/README.print.html" "$(BUILD_DIR)/README.pdf"
	$(RM) "$(BUILD_DIR)/user-guide.css" $(GUIDE_ASSETS) $(MATH_ASSETS)

clean: clean-user-guide
	$(RM) Tutorial.html Tutorial.pdf Instructions.html Instructions.pdf *.xhtml *.fo *~

%.xml: %.tut.xml
	./add_screen.py < $^ > $@

%.html: %.xml docbook-html.xsl
	xsltproc --nonet --xinclude docbook-html.xsl $*.xml > $@

%.html: %.md
	pandoc -s $*.md --css pandoc.css --toc > $@

%.pdf: %.html
	weasyprint $*.html $*.pdf
