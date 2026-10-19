all: user-guide tutorial instructions

# Run from doc/; build directories contain only generated outputs.
TUTORIAL_BUILD_DIR ?= _build/tutorial
INSTRUCTIONS_BUILD_DIR ?= _build/instructions
# Dependency setup is explicit; ordinary builds never install packages.
BUILD_DIR ?= _build/user-guide
TOOLS_DIR ?= _tools
DOCBOOK_XSLTNG ?= $(TOOLS_DIR)/docbook-xslTNG-2.8.4
NODE_MODULES = $(abspath $(TOOLS_DIR)/npm/node_modules)
export NODE_PATH = $(NODE_MODULES)
ASSET_DIR = $(BUILD_DIR)/user-guide-assets
GUIDE_IMAGES = $(addprefix $(BUILD_DIR)/guide-images/,rate-heterotachy.png rate-viewer.png)
GUIDE_ASSETS = $(addprefix $(ASSET_DIR)/,docbook.css docbook-paged.css docbook-toc.css \
               persistent-toc.js guide-toc.js draft-left.svg draft-right.svg LICENSE)
MATH_ASSETS = $(addprefix $(ASSET_DIR)/,mathjax-config.js tex-svg.js MathJax-LICENSE)

.DELETE_ON_ERROR:
.PHONY: all user-guide UserGuide.html UserGuide.pdf UserGuide-A4.pdf clean clean-user-guide setup-user-guide check-user-guide tutorial Tutorial.html Tutorial.pdf clean-tutorial instructions Instructions.html Instructions.pdf clean-instructions

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

user-guide: $(BUILD_DIR)/UserGuide.html $(BUILD_DIR)/UserGuide.pdf $(BUILD_DIR)/UserGuide-A4.pdf
UserGuide.html: $(BUILD_DIR)/UserGuide.html
UserGuide.pdf: $(BUILD_DIR)/UserGuide.pdf
UserGuide-A4.pdf: $(BUILD_DIR)/UserGuide-A4.pdf

$(BUILD_DIR)/UserGuide.html: $(DOCBOOK_XSLTNG)/bin/docbook $(NODE_MODULES)/mathjax-full/package.json \
                        UserGuide.xml user-guide.xsl $(GUIDE_IMAGES) $(GUIDE_ASSETS) $(MATH_ASSETS) $(BUILD_DIR)/user-guide.css \
                        $(DOCBOOK_XSLTNG)/xslt/catalog.xml $(DOCBOOK_XSLTNG)/xslt/docbook.xsl
	@command -v pygmentize >/dev/null || { echo 'User Guide requires pygmentize (Pygments).' >&2; exit 1; }
	"$(DOCBOOK_XSLTNG)/bin/docbook" -catalog:"$(DOCBOOK_XSLTNG)/xslt/catalog.xml" \
	    -xi:on -s:UserGuide.xml -xsl:user-guide.xsl -o:"$@"

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

$(BUILD_DIR)/guide-images/%.png: guide-images/%.png
	mkdir -p "$(BUILD_DIR)/guide-images"
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

$(BUILD_DIR)/UserGuide.print.html: $(BUILD_DIR)/UserGuide.html render-math-svg.cjs mathjax-config.js $(NODE_MODULES)/mathjax-full/package.json
	node render-math-svg.cjs "$<" "$@"

$(BUILD_DIR)/UserGuide.pdf: $(BUILD_DIR)/UserGuide.print.html \
                       $(BUILD_DIR)/user-guide.css $(GUIDE_ASSETS)
	weasyprint "$<" "$@"

$(BUILD_DIR)/UserGuide-A4.pdf: $(BUILD_DIR)/UserGuide.print.html \
                          $(BUILD_DIR)/user-guide.css $(GUIDE_ASSETS) user-guide-A4.css
	weasyprint --stylesheet user-guide-A4.css "$<" "$@"

tutorial: $(TUTORIAL_BUILD_DIR)/Tutorial.html $(TUTORIAL_BUILD_DIR)/Tutorial.pdf
Tutorial.html: $(TUTORIAL_BUILD_DIR)/Tutorial.html $(TUTORIAL_BUILD_DIR)/guide.css $(TUTORIAL_BUILD_DIR)/tutorial.css
Tutorial.pdf: $(TUTORIAL_BUILD_DIR)/Tutorial.pdf

$(TUTORIAL_BUILD_DIR)/Tutorial.xml: Tutorial.tut.xml add_screen.py
	mkdir -p "$(TUTORIAL_BUILD_DIR)"
	./add_screen.py < "$<" > "$@"

$(TUTORIAL_BUILD_DIR)/Tutorial.html: $(TUTORIAL_BUILD_DIR)/Tutorial.xml docbook-html.xsl GNUmakefile
	xsltproc --nonet --xinclude --stringparam html.stylesheet "guide.css tutorial.css" docbook-html.xsl "$<" > "$@"

$(TUTORIAL_BUILD_DIR)/%.css: %.css
	mkdir -p "$(TUTORIAL_BUILD_DIR)"
	cp "$<" "$@"

$(TUTORIAL_BUILD_DIR)/Tutorial.pdf: $(TUTORIAL_BUILD_DIR)/Tutorial.html $(TUTORIAL_BUILD_DIR)/guide.css $(TUTORIAL_BUILD_DIR)/tutorial.css
	weasyprint "$<" "$@"

clean-tutorial:
	$(RM) "$(TUTORIAL_BUILD_DIR)/Tutorial.xml" "$(TUTORIAL_BUILD_DIR)/Tutorial.html" \
	    "$(TUTORIAL_BUILD_DIR)/Tutorial.pdf" "$(TUTORIAL_BUILD_DIR)/guide.css" "$(TUTORIAL_BUILD_DIR)/tutorial.css"

instructions: $(INSTRUCTIONS_BUILD_DIR)/Instructions.html $(INSTRUCTIONS_BUILD_DIR)/Instructions.pdf $(INSTRUCTIONS_BUILD_DIR)/guide.css
Instructions.html: $(INSTRUCTIONS_BUILD_DIR)/Instructions.html $(INSTRUCTIONS_BUILD_DIR)/guide.css
Instructions.pdf: $(INSTRUCTIONS_BUILD_DIR)/Instructions.pdf

$(INSTRUCTIONS_BUILD_DIR)/Instructions.xml: Instructions.tut.xml add_screen.py
	mkdir -p "$(INSTRUCTIONS_BUILD_DIR)"
	./add_screen.py < "$<" > "$@"

$(INSTRUCTIONS_BUILD_DIR)/Instructions.html: $(INSTRUCTIONS_BUILD_DIR)/Instructions.xml docbook-html.xsl GNUmakefile
	xsltproc --nonet --xinclude docbook-html.xsl "$<" > "$@"

$(INSTRUCTIONS_BUILD_DIR)/guide.css: guide.css
	mkdir -p "$(INSTRUCTIONS_BUILD_DIR)"
	cp "$<" "$@"

$(INSTRUCTIONS_BUILD_DIR)/Instructions.pdf: $(INSTRUCTIONS_BUILD_DIR)/Instructions.html $(INSTRUCTIONS_BUILD_DIR)/guide.css
	weasyprint "$<" "$@"

clean-instructions:
	$(RM) "$(INSTRUCTIONS_BUILD_DIR)/Instructions.xml" "$(INSTRUCTIONS_BUILD_DIR)/Instructions.html" \
	    "$(INSTRUCTIONS_BUILD_DIR)/Instructions.pdf" "$(INSTRUCTIONS_BUILD_DIR)/guide.css"

# Clean only named outputs; retain sources and downloaded tools.
clean-user-guide:
	$(RM) "$(BUILD_DIR)/UserGuide.html" "$(BUILD_DIR)/UserGuide.print.html" "$(BUILD_DIR)/UserGuide.pdf" "$(BUILD_DIR)/UserGuide-A4.pdf"
	$(RM) "$(BUILD_DIR)/user-guide.css" $(GUIDE_IMAGES) $(GUIDE_ASSETS) $(MATH_ASSETS)

clean: clean-user-guide clean-tutorial clean-instructions clean-model-reference

# Generate portable Markdown; the website supplies its own Pandoc template.
REFERENCE_BUILD_DIR ?= _build/model-reference
.PHONY: model-reference clean-model-reference
model-reference:
	python3 generate-reference.py "$(REFERENCE_BUILD_DIR)"

# The reference generator owns this directory; keep it separate from handwritten sources.
clean-model-reference:
	$(RM) -r "$(REFERENCE_BUILD_DIR)"
