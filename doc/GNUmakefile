all: README.html README.xhtml README.pdf Tutorial.xhtml Tutorial.html Tutorial.pdf Instructions.xhtml Instructions.pdf

clean:
	-@rm -f *.html *.xhtml REAMDE.xml *.fo *.pdf *~


%.xml: %.itex.xml
	itex2MML < $^ > $@

%.xml: %.tut.xml
	./add_screen.pl < $^ > $@

%.html: %.xml docbook-html.xsl
	xsltproc --nonet --xinclude docbook-html.xsl $*.xml > $@

%.html: %.md
	pandoc -s $*.md --css pandoc.css --toc > $@

%.xhtml: %.xml docbook-xhtml.xsl
	xsltproc --nonet --xinclude docbook-xhtml.xsl $*.xml > $@

%.pdf: %.xhtml
	weasyprint $*.xhtml $*.pdf

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml
