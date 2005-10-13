all: README.xhtml README.pdf 

clean:
	-@rm -f README.xhtml README.xml README.fo README.pdf *~


%.xml: %.itex.xml
	itex2MML < $^ > $@

%.xhtml: %.xml docbook-xhtml.xsl
	xsltproc --xinclude docbook-xhtml.xsl $*.xml > $@

%.fo: %.xml
	xsltproc --xinclude docbook-fo.xsl $*.xml > $@

%.pdf: %.fo
	fop $*.fo $*.pdf

%.txt: %.xml
	fop $*.fo -txt $*.txt

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml