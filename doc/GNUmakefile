all: README.html


%.html: %.xml
	xsltproc --catalogs /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl $^ > $@