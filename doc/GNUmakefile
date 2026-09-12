all: user-guide Tutorial.html Tutorial.pdf Instructions.html Instructions.pdf

# Run from doc/; BUILD_DIR contains only generated User Guide outputs.
# Dependency setup is explicit; ordinary builds never install packages.
BUILD_DIR ?= ../../build/user-guide
TOOLS_DIR ?= ../../build/user-guide-tools
DOCBOOK_XSLTNG ?= $(TOOLS_DIR)/docbook-xslTNG-2.8.4
NODE_MODULES = $(abspath $(TOOLS_DIR)/npm/node_modules)
export NODE_PATH = $(NODE_MODULES)
ASSET_DIR = $(BUILD_DIR)/user-guide-assets
GUIDE_ASSETS = $(addprefix $(ASSET_DIR)/,docbook.css docbook-paged.css docbook-toc.css \
               persistent-toc.js guide-toc.js draft-left.svg draft-right.svg LICENSE)
MATH_ASSETS = $(addprefix $(ASSET_DIR)/,mathjax-config.js tex-svg.js MathJax-LICENSE)

.DELETE_ON_ERROR:
.PHONY: all user-guide README.html README.pdf README-A4.pdf clean clean-user-guide setup-user-guide check-user-guide

setup-user-guide:
	python3 setup-guide.py "$(TOOLS_DIR)" "$(DOCBOOK_XSLTNG)"

# These files are supplied by setup or an existing installation; builds do not download tools.
$(DOCBOOK_XSLTNG)/bin/docbook $(NODE_MODULES)/mathjax-full/package.json \
$(NODE_MODULES)/mathjax-full/es5/tex-svg.js $(NODE_MODULES)/mathjax-full/LICENSE:
	@echo 'Missing $@. Run make -C doc setup-user-guide or set DOCBOOK_XSLTNG to an installed release.' >&2
	@exit 1

check-user-guide: user-guide
	node test-render-math-svg.cjs
	node check-guide.cjs "$(BUILD_DIR)"

user-guide: $(BUILD_DIR)/README.html $(BUILD_DIR)/README.pdf $(BUILD_DIR)/README-A4.pdf
README.html: $(BUILD_DIR)/README.html
README.pdf: $(BUILD_DIR)/README.pdf
README-A4.pdf: $(BUILD_DIR)/README-A4.pdf

$(BUILD_DIR)/README.html: $(DOCBOOK_XSLTNG)/bin/docbook $(NODE_MODULES)/mathjax-full/package.json \
                        README.xml user-guide.xsl $(GUIDE_ASSETS) $(MATH_ASSETS) $(BUILD_DIR)/user-guide.css \
                        $(DOCBOOK_XSLTNG)/xslt/catalog.xml $(DOCBOOK_XSLTNG)/xslt/docbook.xsl
	@command -v pygmentize >/dev/null || { echo 'User Guide requires pygmentize (Pygments).' >&2; exit 1; }
	"$(DOCBOOK_XSLTNG)/bin/docbook" -catalog:"$(DOCBOOK_XSLTNG)/xslt/catalog.xml" \
	    -xi:on -s:README.xml -xsl:user-guide.xsl -o:"$@"

# Strip remote font imports only from the copied stylesheet, preserving upstream CSS.
$(ASSET_DIR)/docbook.css: $(DOCBOOK_XSLTNG)/resources/css/docbook.css
	mkdir -p "$(ASSET_DIR)"
	sed '/^@import url("https:\/\/fonts.googleapis.com\//d' "$<" > "$@"

$(ASSET_DIR)/%: $(DOCBOOK_XSLTNG)/resources/css/%
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(ASSET_DIR)/persistent-toc.js: $(DOCBOOK_XSLTNG)/resources/js/persistent-toc.js
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(ASSET_DIR)/guide-toc.js: guide-toc.js
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

$(ASSET_DIR)/tex-svg.js: $(NODE_MODULES)/mathjax-full/es5/tex-svg.js
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(ASSET_DIR)/MathJax-LICENSE: $(NODE_MODULES)/mathjax-full/LICENSE
	mkdir -p "$(ASSET_DIR)"
	cp "$<" "$@"

$(BUILD_DIR)/README.print.html: $(BUILD_DIR)/README.html render-math-svg.cjs mathjax-config.js $(NODE_MODULES)/mathjax-full/package.json
	node render-math-svg.cjs "$<" "$@"

$(BUILD_DIR)/README.pdf: $(BUILD_DIR)/README.print.html \
                       $(BUILD_DIR)/user-guide.css $(GUIDE_ASSETS)
	weasyprint "$<" "$@"

$(BUILD_DIR)/README-A4.pdf: $(BUILD_DIR)/README.print.html \
                          $(BUILD_DIR)/user-guide.css $(GUIDE_ASSETS) user-guide-A4.css
	weasyprint --stylesheet user-guide-A4.css "$<" "$@"

Tutorial.pdf Instructions.pdf: guide.css

# Name the generated files explicitly: README.xml is now a source file.
clean-user-guide:
	$(RM) "$(BUILD_DIR)/README.html" "$(BUILD_DIR)/README.print.html" "$(BUILD_DIR)/README.pdf" "$(BUILD_DIR)/README-A4.pdf"
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
