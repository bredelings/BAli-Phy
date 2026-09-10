<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:m="http://docbook.org/ns/docbook/modes"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="m"
                version="3.0">

  <!-- The release's XML catalog resolves this import to the local installation. -->
  <xsl:import href="https://cdn.docbook.org/release/xsltng/current/xslt/docbook.xsl"/>

  <xsl:param name="chunk" select="()"/>
  <xsl:param name="section-numbers" select="'true'"/>
  <xsl:param name="persistent-toc" select="'false'"/>
  <xsl:param name="theme-picker" select="'false'"/>

  <!-- Keep command whitespace and inline markup, without extra line numbers or controls. -->
  <xsl:param name="verbatim-line-style" select="''"/>
  <xsl:param name="verbatim-plain-style"
             select="'address literallayout funcsynopsisinfo classsynopsisinfo
                      programlisting programlistingco screen screenco synopsis'"/>
  <xsl:param name="verbatim-numbered-elements" select="''"/>
  <xsl:param name="verbatim-syntax-highlighter" select="'none'"/>
  <xsl:param name="verbatim-syntax-highlight-languages" select="''"/>
  <xsl:param name="verbatim-embellishments" select="'false'"/>
  <xsl:param name="verbatim-trim-trailing-blank-lines" select="'false'"/>

  <!-- Both browser viewing and PDF generation use the same HTML. Keep the upstream
       CSS intact, including its web fonts, and leave MathJax at its default. -->
  <xsl:param name="use-docbook-css" select="'false'"/>
  <xsl:template match="*" mode="m:html-head-links">
    <link rel="stylesheet" href="user-guide-assets/docbook.css" media="screen"/>
    <link rel="stylesheet" href="user-guide-assets/docbook-paged.css" media="print"/>
    <link rel="stylesheet" href="user-guide.css"/>
  </xsl:template>

</xsl:stylesheet>
