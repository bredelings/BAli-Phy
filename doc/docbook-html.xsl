<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:h="http://www.w3.org/1999/xhtml"
                xmlns:exsl="http://exslt.org/common"
                exclude-result-prefixes="doc mml h exsl"
                version='1.0'>

<xsl:import href="/usr/share/xml/docbook/stylesheet/docbook-xsl-ns/xhtml5/docbook.xsl"/>
<xsl:output method="html" encoding="UTF-8" omit-xml-declaration="yes"/>

<!-- DocBook's XHTML5 tree needs unnamespaced HTML elements for libxslt's HTML serializer
     to handle void elements correctly. Keep foreign content (especially MathML) unchanged.
     This pass is unnecessary if the upstream stylesheet gains HTML serialization support. -->
<xsl:template match="/">
  <xsl:variable name="page"><xsl:apply-imports/></xsl:variable>
  <xsl:text disable-output-escaping="yes">&lt;!DOCTYPE html&gt;</xsl:text>
  <xsl:apply-templates select="exsl:node-set($page)/node()" mode="serialize-html"/>
</xsl:template>

<!-- Emit the doctype only after the intermediate tree has been converted. -->
<xsl:template name="user.preroot"/>

<!-- Keep the source language available to browsers and assistive technology. -->
<xsl:template name="root.attributes">
  <xsl:if test="/*/@xml:lang">
    <xsl:attribute name="lang"><xsl:value-of select="/*/@xml:lang"/></xsl:attribute>
  </xsl:if>
</xsl:template>

<!-- Only HTML loses its namespace; attributes and child nodes retain their values. -->
<xsl:template match="h:*" mode="serialize-html">
  <xsl:element name="{local-name()}" namespace="">
    <!-- XHTML5 puts table classes on wrappers; retain the existing table CSS selectors. -->
    <xsl:if test="self::h:table and not(@class) and
                  (../@class = 'informaltable' or ../@class = 'table')">
      <xsl:attribute name="class"><xsl:value-of select="../@class"/></xsl:attribute>
    </xsl:if>
    <xsl:apply-templates select="@*|node()" mode="serialize-html"/>
  </xsl:element>
</xsl:template>

<!-- Preserve non-HTML content, including MathML namespaces and mathematical structure. -->
<xsl:template match="@*|node()" mode="serialize-html">
  <xsl:copy>
    <xsl:apply-templates select="@*|node()" mode="serialize-html"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="h:*/@xml:lang" mode="serialize-html"/>

<!-- MathML support can be so easy ;-) ... -->
<xsl:template match="mml:math">
  <xsl:copy-of select="."/>
</xsl:template>
<!-- end of MathML support. -->

<xsl:param name="html.stylesheet">guide.css</xsl:param>
<!-- Use our existing CSS; do not generate an upstream docbook.css over it. -->
<xsl:param name="docbook.css.source" select="''"/>
<xsl:param name="html.ext">.html</xsl:param>
<xsl:param name="toc.list.type">dl</xsl:param>
<xsl:param name="variablelist.as.table" select="1"/>
<xsl:param name="section.autolabel" select="1"/>
<!-- xsl:param name="admon.graphics" select="1"/ -->


</xsl:stylesheet>
