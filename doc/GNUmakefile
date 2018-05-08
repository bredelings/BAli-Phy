all: README.html README.xhtml README.pdf Tutorial.html Tutorial.xhtml Tutorial.pdf Tutorial2.html Tutorial2.xhtml Tutorial2.pdf Tutorial3.html Tutorial3.xhtml Tutorial3.pdf Tutorial4.html Tutorial4.xhtml Tutorial4.pdf

clean:
	-@rm -f *.html *.xhtml REAMDE.xml *.fo *.pdf *~


%.xml: %.itex.xml
	itex2MML < $^ > $@

%.xml: %.tut.xml
	./add_screen.pl < $^ > $@

%.html: %.xml docbook-html.xsl
	xsltproc --nonet --xinclude docbook-html.xsl $*.xml > $@

%.xhtml: %.xml docbook-xhtml.xsl
	xsltproc --nonet --xinclude docbook-xhtml.xsl $*.xml > $@

%.pdf: %.xhtml
	wkhtmltopdf -B 10 -T 10 -R 10 -L 10 -s Letter $*.xhtml $*.pdf

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml
