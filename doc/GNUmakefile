all: README.html


STYLEBASE=/usr/share/xml/docbook/stylesheet/nwalsh

%.html: %.xml
	xsltproc --stringparam html.stylesheet docbook.css --catalogs ${STYLEBASE}/xhtml/docbook.xsl $^ > $@

%.pdf: %.xml
	xmlto pdf $^

#xmlto html %.xml

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml