all: README.xhtml

clean:
	-@rm -f README.xhtml README.xml README.fo *~


%.xml: %.itex.xml
	itex2MML < $^ > $@

%.xhtml: %.xml docbook.xsl
	xsltproc --xinclude docbook.xsl $*.xml > $@

%.pdf: %.xml
	fop $*.xml $*.pdf

%.txt: %.xml
	xmlto txt $^

%.fo: %.xml
	xmlto fo $^

#xmlto html %.xml

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml