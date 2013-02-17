all: README.html README.xhtml README.pdf Tutorial.html Tutorial.xhtml Tutorial.pdf

clean:
	-@rm -f README.xhtml README.xml README.fo README.pdf *~


%.xml: %.itex.xml
	itex2MML < $^ > $@

%.xml: %.tut.xml
	add_screen.pl < $^ > $@

%.html: %.xml docbook-html.xsl
	xsltproc --xinclude docbook-html.xsl $*.xml > $@

%.xhtml: %.xml docbook-xhtml.xsl
	xsltproc --xinclude docbook-xhtml.xsl $*.xml > $@

%.fo: %.xml docbook-fo.xsl
	xsltproc --xinclude docbook-fo.xsl $*.xml > $@

%.pdf: %.fo
	fop $*.fo $*.pdf

%.txt: %.xml
	fop $*.fo -txt $*.txt

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml
