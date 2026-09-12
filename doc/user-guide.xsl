<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:m="http://docbook.org/ns/docbook/modes"
                xmlns:db="http://docbook.org/ns/docbook"
                xmlns:f="http://docbook.org/ns/docbook/functions"
                xmlns:h="http://www.w3.org/1999/xhtml"
                xmlns:t="http://docbook.org/ns/docbook/templates"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="m db f h t"
                version="3.0">

  <!-- The release's XML catalog resolves this import to the local installation. -->
  <xsl:import href="https://cdn.docbook.org/release/xsltng/current/xslt/docbook.xsl"/>

  <xsl:param name="chunk" select="()"/>
  <xsl:param name="section-numbers" select="'true'"/>
  <xsl:param name="persistent-toc" select="'true'"/>
  <xsl:param name="persistent-toc-search" select="'true'"/>
  <!-- Embed the panel contents so the guide also works directly from a local file. -->
  <xsl:param name="persistent-toc-filename" select="''"/>
  <xsl:param name="persistent-toc-css" select="'user-guide-assets/docbook-toc.css'"/>
  <xsl:param name="persistent-toc-js" select="'user-guide-assets/persistent-toc.js'"/>
  <xsl:param name="theme-picker" select="'false'"/>

  <!-- Keep command whitespace and inline markup, without extra line numbers or controls. -->
  <xsl:param name="verbatim-line-style" select="''"/>
  <xsl:param name="verbatim-plain-style"
             select="'address literallayout funcsynopsisinfo classsynopsisinfo
                      programlisting programlistingco screen screenco synopsis'"/>
  <xsl:param name="verbatim-numbered-elements" select="''"/>
  <xsl:param name="verbatim-syntax-highlighter" select="'pygments'"/>
  <xsl:param name="verbatim-syntax-highlight-languages" select="'bali-phy-model'"/>
  <xsl:param name="verbatim-syntax-highlight-css" select="''"/>
  <xsl:param name="verbatim-embellishments" select="'false'"/>
  <xsl:param name="copy-verbatim-js" select="''"/>
  <xsl:param name="verbatim-trim-leading-blank-lines" select="'false'"/>
  <xsl:param name="verbatim-trim-trailing-blank-lines" select="'false'"/>

  <!-- File examples need an unnumbered caption, while the existing listing renderer
       continues to handle whitespace, replacement text, and syntax highlighting. -->
  <xsl:template match="db:example[@role='file-content']" mode="m:docbook">
    <figure>
      <xsl:apply-templates select="." mode="m:attributes"/>
      <figcaption>
        <xsl:apply-templates select="(db:title|db:info/db:title)/node()" mode="m:docbook"/>
      </figcaption>
      <xsl:apply-templates select="* except (db:title, db:info)" mode="m:docbook"/>
    </figure>
  </xsl:template>

  <!-- Use the standard inline renderer to retain links, IDs, and replaceable markup,
       adding a language class so inline expressions share the listing's token colors. -->
  <xsl:template match="db:code[@language='bali-phy-model']" mode="m:docbook">
    <xsl:call-template name="t:inline">
      <xsl:with-param name="namemap" select="'code'"/>
      <xsl:with-param name="local-name-as-class" select="false()"/>
      <xsl:with-param name="class" select="'language-bali-phy-model'"/>
    </xsl:call-template>
  </xsl:template>

  <!-- Bare inline identifiers have no token colors; avoid a Pygments process for each one.
       Higher priority keeps these and whitespace-only text out of the highlighting template. -->
  <xsl:template match="db:code[@language='bali-phy-model']/text()
                       [matches(., '^\s*([A-Za-z_][A-Za-z_0-9.]*)?\s*$')]"
                mode="m:docbook" priority="2">
    <xsl:value-of select="."/>
  </xsl:template>

  <!-- Highlight direct text only, leaving nested replaceable elements in their original style.
       Avoid xslTNG's line embellishments, which insert spaces on blank lines.
       Supply a final newline so the lexer recognizes a final // comment, then remove it
       from the formatted result only if absent in the source.
       Java approximates the model language's strings, numbers, operators, and comments;
       replace it if a dedicated BAli-Phy model-language lexer becomes available. -->
  <xsl:template match="db:programlisting[@language='bali-phy-model']/text()
                       |db:code[@language='bali-phy-model']/text()" mode="m:docbook">
    <xsl:variable name="tokens" select="f:syntax-highlight(string(.),
                   map { 'language': 'java' }, map { 'stripnl': 'false', 'ensurenl': 'true' })"/>
    <!-- Java treats square brackets as operators. Split only operator spans so brackets
         become grouping punctuation without recoloring symbols inside strings or comments. -->
    <xsl:variable name="highlighted" as="node()*">
      <xsl:for-each select="$tokens">
        <xsl:choose>
          <xsl:when test="self::h:span[@class='o']">
            <xsl:analyze-string select="string(.)" regex="\[|\]">
              <xsl:matching-substring><span class="p"><xsl:value-of select="."/></span></xsl:matching-substring>
              <xsl:non-matching-substring><span class="o"><xsl:value-of select="."/></span></xsl:non-matching-substring>
            </xsl:analyze-string>
          </xsl:when>
          <xsl:otherwise><xsl:sequence select="."/></xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="trim" select="not(ends-with(., '&#10;'))
                   and $highlighted[last()] instance of text()
                   and ends-with(string($highlighted[last()]), '&#10;')"/>
    <xsl:sequence select="$highlighted[position() lt last()]"/>
    <xsl:choose>
      <xsl:when test="$trim">
        <xsl:value-of select="substring(string($highlighted[last()]), 1,
                                        string-length(string($highlighted[last()])) - 1)"/>
      </xsl:when>
      <xsl:otherwise><xsl:sequence select="$highlighted[last()]"/></xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Both browser viewing and PDF generation use the same HTML. Keep the upstream
       CSS intact. Load MathJax explicitly because TeX text does not trigger xslTNG math detection. -->
  <xsl:param name="use-docbook-css" select="'false'"/>
  <xsl:template match="*" mode="m:html-head-links">
    <link rel="stylesheet" href="user-guide-assets/docbook.css" media="screen"/>
    <link rel="stylesheet" href="user-guide-assets/docbook-paged.css" media="print"/>
    <link rel="stylesheet" href="user-guide.css"/>
    <script src="user-guide-assets/mathjax-config.js"></script>
    <script defer="defer" src="user-guide-assets/tex-svg.js"></script>
    <script defer="defer" src="user-guide-assets/guide-toc.js"></script>
  </xsl:template>

</xsl:stylesheet>
