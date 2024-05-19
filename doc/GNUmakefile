all: README.html README.xhtml README.pdf Tutorial6.xhtml Tutorial6.html Tutorial6.pdf Instructions8.xhtml Instructions8.pdf

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
	wkhtmltopdf --enable-local-file-access -B 10 -T 10 -R 10 -L 10 -s Letter $*.xhtml $*.pdf

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml
