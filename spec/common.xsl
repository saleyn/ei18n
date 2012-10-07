<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" media-type="text/plain" encoding="utf-8"/>

<xsl:template name="maximum">
    <xsl:param name="values"/>
    <xsl:for-each select="$values">
        <xsl:sort select="string-length(.)" data-type="number" order="descending"/>
        <xsl:if test="position()=1"><xsl:value-of select="string-length(.)"/></xsl:if>
    </xsl:for-each>
</xsl:template>

<xsl:template name="pad">
    <xsl:param name="padChar" select="' '"/>
    <xsl:param name="padCount" select="0"/>
    <xsl:if test="$padCount &gt; 0">
        <xsl:value-of select="$padChar"/>
    </xsl:if>
    <xsl:if test="$padCount &gt; 1">
        <xsl:call-template name="pad">
            <xsl:with-param name="padCount" select="number($padCount) - 1"/>
            <xsl:with-param name="padChar" select="$padChar"/>
        </xsl:call-template>
    </xsl:if>
</xsl:template>
 
</xsl:stylesheet>
