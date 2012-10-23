<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" media-type="text/plain" encoding="utf-8"/>

<xsl:param name="now"/>
<xsl:param name="xslfile"/>
<xsl:param name="xmlfile"/>
<xsl:param name="user"/>
<xsl:param name="email"/>

<xsl:include href="common.xsl"/>

<xsl:template match="/translations">
<xsl:text>%%%----------------------------------------------------------------------------
%%% Internationalization text constants
%%%
%%% Copyright (c) 2012 Serge Aleynikov &lt;saleyn@gmail.com>
%%%----------------------------------------------------------------------------
%%%      Version: </xsl:text><xsl:value-of select="/translations/@version"/><xsl:text>
%%% Generated by: </xsl:text><xsl:value-of select="$user"/>
<xsl:if test="$email">
    <xsl:text> &lt;</xsl:text><xsl:value-of select="$email"/><xsl:text>&gt;</xsl:text>
</xsl:if>
<xsl:text>
%%%      Created: </xsl:text><xsl:value-of select="$now"/><xsl:text>
%%%----------------------------------------------------------------------------
%%%
%%% This file is automatically generated by
%%% </xsl:text><xsl:value-of select="$xslfile"/><xsl:text> from </xsl:text>
    <xsl:value-of select="$xmlfile"/><xsl:text>
%%%
%%%            >>>>>>>  !!! DO NOT MODIFY BY HAND  !!! &lt;&lt;&lt;&lt;&lt;&lt;&lt;
%%%
%%%----------------------------------------------------------------------------

</xsl:text>

<xsl:variable name="max-title-len">
    <xsl:call-template name="maximum">
        <xsl:with-param name="values" select="lang[@iso = /translations/@default-lang]/text/@name"/>
        <xsl:with-param name="add" select="5"/>
    </xsl:call-template>
</xsl:variable>

<xsl:for-each select="lang[@iso = /translations/@default-lang]/text[@type = 'static']">
    <xsl:variable name="title">
        <xsl:choose>
            <xsl:when test="@name">
                <xsl:call-template name="format-const-name">
                    <xsl:with-param name="text" select="@name"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:when test="@value">
                <xsl:call-template name="format-const-name">
                    <xsl:with-param name="text" select="@value"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="format-const-name">
                    <xsl:with-param name="text" select="self::node()/text()"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>

    <xsl:text>-define(</xsl:text>
    <xsl:value-of select="$title"/>
    <xsl:text>, </xsl:text>

    <xsl:call-template name="pad">
        <xsl:with-param name="padCount">
            <xsl:value-of select="$max-title-len + 5 - string-length($title)"/>
        </xsl:with-param>
    </xsl:call-template>

    <xsl:value-of select="position()"/>
    <xsl:text>).&#10;</xsl:text>
</xsl:for-each>

</xsl:template>
   
<xsl:template name="format-const-name">
    <xsl:param name="text"/>
    <xsl:param name="width"/>
    
    <xsl:variable name="smallcase" select="'abcdefghijklmnopqrstuvwxyz'" />
    <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

    <xsl:text>I18N_</xsl:text>
    <xsl:value-of select="
        translate(
            translate(
                translate(substring($text, 1, 40), $smallcase, $uppercase),
                '&amp;@#?!$()[]{}&quot;', ''),
            '- ', '__')
    "/>
</xsl:template>

</xsl:stylesheet>
