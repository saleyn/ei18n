<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" media-type="text/plain" encoding="utf-8"/>

<xsl:param name="lang"/>
<xsl:param name="modules"/>
<xsl:param name="eol"/>

<xsl:template match="/translations">
    <xsl:for-each select="lang">
        <xsl:if test="$lang">
            <xsl:text> </xsl:text>
            <xsl:value-of select="@iso"/>
        </xsl:if>

        <xsl:if test="$modules">
            <xsl:text> i18n_trans_</xsl:text>
            <xsl:value-of select="@iso"/>
        </xsl:if>
        
        <xsl:if test="$eol">
            <xsl:text>&#10;</xsl:text>
        </xsl:if>
    </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
