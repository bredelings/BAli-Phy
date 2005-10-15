<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                exclude-result-prefixes="doc mml"
                version='1.0'>

<xsl:import href="/usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl"/>

<!-- MathML support can be so easy ;-) ... -->
<xsl:template match="mml:math">
  <xsl:copy-of select="."/>
</xsl:template>
<!-- end of MathML support. -->

<xsl:param name="html.stylesheet">docbook.css</xsl:param>
<xsl:param name="variablelist.as.table" select="1"/>
<xsl:param name="section.autolabel" select="1"/>
<!-- xsl:param name="admon.graphics" select="1"/ -->


</xsl:stylesheet>
