all:  git.html building.html code.html architecture.png architecture.svg code-layout.png code-layout.svg \
      add_function.html builtin.html distribution.html mcmc_move.html binding.html tests.html

clean:
	-@rm -f *.html *.xhtml REAMDE.xml *.fo *.pdf *~


%.svg: %.dot
	dot -Tsvg < $^ > $@

%.png: %.dot
	dot -Tpng < $^ > $@

%.xml: %.tut.xml
	./add_screen.pl < $^ > $@

%.html: %.xml docbook-html.xsl
	xsltproc --nonet --xinclude docbook-html.xsl $*.xml > $@

# Removed arguments --toc -N 
%.html: %.md
	pandoc -s $*.md --css pandoc.css > $@

%.xhtml: %.xml docbook-xhtml.xsl
	xsltproc --nonet --xinclude docbook-xhtml.xsl $*.xml > $@

%.pdf: %.xhtml
	wkhtmltopdf -B 10 -T 10 -R 10 -L 10 -s Letter $*.xhtml $*.pdf

#xalan -XSLT /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl -IN %.xml
