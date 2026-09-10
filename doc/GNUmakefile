all: README.html README.pdf Tutorial.html Tutorial.pdf Instructions.html Instructions.pdf

README.pdf Tutorial.pdf Instructions.pdf: guide.css

clean:
	-@rm -f *.html *.xhtml REAMDE.xml *.fo *.pdf *~


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
