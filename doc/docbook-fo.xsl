<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
		xmlns:fo="http://www.w3.org/1999/XSL/Format"
                exclude-result-prefixes="doc mml"
                version='1.0'>

<xsl:import href="/usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl"/>

<!-- See tcc_fo.xsl on web -->

<!-- MathML support can be so easy ;-) ... -->
<xsl:template match="mml:math">
  <xsl:copy-of select="."/>
</xsl:template>
<!-- end of MathML support. -->

<xsl:param name="variablelist.as.table" select="1"/>
<xsl:param name="section.autolabel" select="1"/>
<!-- xsl:param name="admon.graphics" select="1"/ -->

<xsl:param name="toc.indent.width" select="5"/>
<xsl:param name="body.start.indent" select="0"/>

<!-- In order to get MathML processed by Jeuclid, it has
     to be wrapped in an fo:instream-foreign-object tag -->
<xsl:template match="mml:math"
xmlns:mml="http://www.w3.org/1998/Math/MathML">
  <xsl:choose>
    <!-- * If user is using passivetex, we don't wrap the output in -->
    <!-- * fo:instream-foreign-object (which passivetex doesn't support).
-->
    <xsl:when test="not($passivetex.extensions = 0)">
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates/>
      </xsl:copy>
    </xsl:when>
    <xsl:otherwise>
      <fo:instream-foreign-object>

        <!-- Support for two imagedata attributes -->
        <xsl:if test="../@align">
          <xsl:attribute name="text-align">
            <xsl:value-of select="../@align"/>
          </xsl:attribute>
        </xsl:if>
       
        <xsl:if test="../@valign">
          <xsl:attribute name="display-align">
            <xsl:choose>
              <xsl:when test="../@valign = 'top'">before</xsl:when>
              <xsl:when test="../@valign = 'middle'">center</xsl:when>
              <xsl:when test="../@valign = 'bottom'">after</xsl:when>
              <xsl:otherwise>auto</xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </xsl:if>
     <!-- End of customization -->
       
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates/>
        </xsl:copy>
      </fo:instream-foreign-object>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="inline.italicsansseq">
  <xsl:param name="content">
    <xsl:apply-templates/>
  </xsl:param>
  <fo:inline font-style="italic" font-family="sans-serif">
    <xsl:copy-of select="$content"/>
  </fo:inline>
</xsl:template>

<xsl:template name="inline.smallcaps">
  <xsl:param name="content">
    <xsl:apply-templates/>
  </xsl:param>
  <fo:inline font-variant="small-caps">
    <xsl:copy-of select="$content"/>
  </fo:inline>
</xsl:template>

<xsl:template
    match="application|guibutton|guiicon|guilabel|guimenu">
  <xsl:call-template name="inline.italicsansseq"/>
</xsl:template>

<xsl:template match="firstterm">
  <xsl:call-template name="inline.italicseq"/>
</xsl:template>

<xsl:template match="keysym">
  <xsl:call-template name="inline.italicsansseq"/>
</xsl:template>
<xsl:param name="shade.verbatim" select="1"></xsl:param>
<xsl:param name="variablelist.as.blocks" select="1"></xsl:param>
<xsl:template match="varlistentry/term">
  <xsl:call-template name="inline.boldseq"/>
</xsl:template>
</xsl:stylesheet>
