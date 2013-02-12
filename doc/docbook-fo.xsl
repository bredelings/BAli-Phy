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

<xsl:param name="double.sided">1</xsl:param>
<xsl:param name="page.margin.outer">0.75in</xsl:param>
<xsl:param name="page.margin.inner">1in</xsl:param>
<xsl:param name="insert.xref.page.number">1</xsl:param>
<xsl:param name="draft.mode">no</xsl:param>
<xsl:param name="region.before.extent">0.25in</xsl:param>
<xsl:attribute-set name="normal.para.spacing">
  <xsl:attribute name="space-before.minimum">0.50em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">0.60em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.70em</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="list.block.spacing">
  <xsl:attribute name="space-before.minimum">0.70em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">0.75em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.80em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.70em</xsl:attribute>
  <xsl:attribute name="space-after.optimum">0.75em</xsl:attribute>
  <xsl:attribute name="space-after.maximum">0.80em</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="list.item.spacing">
  <xsl:attribute name="space-before.minimum">0.50em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">0.60em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.70em</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="verbatim.properties">
  <xsl:attribute name="space-before.minimum">0.4em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">0.5em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.6em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.4em</xsl:attribute>
  <xsl:attribute name="space-after.optimum">0.5em</xsl:attribute>
  <xsl:attribute name="space-after.maximum">0.6em</xsl:attribute>
  <xsl:attribute name="border-width">0.1mm</xsl:attribute>
  <xsl:attribute name="border-style">solid</xsl:attribute>

  <xsl:attribute name="padding">1mm</xsl:attribute>
</xsl:attribute-set>
<xsl:template
    match="filename|sgmltag|userinput|varname|application"
    mode="no.anchor.mode">
  <xsl:apply-templates select="." />
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

<xsl:template name="nongraphical.admonition">
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <fo:block space-before.minimum="0.8em"
            space-before.optimum="1em"
            space-before.maximum="1.2em"
            start-indent="0.25in"
            end-indent="0.25in"
            border="4pt solid #d0d0d0"
            padding="4pt"
            id="{$id}">
    <xsl:if test="$admon.textlabel != 0 or title">
      <fo:block keep-with-next='always'
                xsl:use-attribute-sets="admonition.title.properties">
         <xsl:apply-templates select="." mode="object.title.markup"/>
      </fo:block>
    </xsl:if>

    <fo:block xsl:use-attribute-sets="admonition.properties">
      <xsl:apply-templates/>
    </fo:block>
  </fo:block>
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
