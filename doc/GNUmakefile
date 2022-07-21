all: README.html README.xhtml README.pdf Tutorial.html Tutorial.xhtml Tutorial.pdf Tutorial2.html Tutorial2.xhtml Tutorial2.pdf Tutorial3.html Tutorial3.xhtml Tutorial3.pdf Tutorial4.html Tutorial4.xhtml Tutorial4.pdf Instructions6.pdf Instructions6.xhtml Instructions7.pdf Instructions7.xhtml

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
