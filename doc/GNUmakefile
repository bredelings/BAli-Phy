all: README.xhtml

clean:
	-@rm -f README.xhtml README.xml README.fo README.pdf *~


%.xml: %.itex.xml
	itex2MML < $^ > $@

%.xhtml: %.xml docbook.xsl
	xsltproc --xinclude docbook.xsl $*.xml > $@

%.fo: %.xml
	xsltproc --xinclude docbook-fo.xsl $*.xml > $@

%.pdf: %.fo
	fop $*.fo $*.pdf

%.txt: %.xml
	xmlto txt $^

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml