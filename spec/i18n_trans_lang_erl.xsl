<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" media-type="text/plain" encoding="utf-8"/>

<xsl:param name="now"/>
<xsl:param name="xslfile"/>
<xsl:param name="xmlfile"/>
<xsl:param name="user"/>
<xsl:param name="email"/>
<xsl:param name="lang">en</xsl:param>

<xsl:include href="common.xsl"/>

<xsl:template match="/translations">
<xsl:text>%%%----------------------------------------------------------------------------
%%% @doc Internationalization text constants for "</xsl:text><xsl:value-of select="$lang"/>
<xsl:text>" language.
%%%
%%% @copyright 2012 Serge Aleynikov &lt;saleyn@gmail.com>
%%% @end
%%%      Version: </xsl:text><xsl:value-of select="/translations/@version"/><xsl:text>
%%% Generated by: </xsl:text><xsl:value-of select="$user"/>
<xsl:if test="$email">
    <xsl:text> &lt;</xsl:text><xsl:value-of select="$email"/><xsl:text>&gt;</xsl:text>
</xsl:if>
<xsl:text>
%%%----------------------------------------------------------------------------
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
-module(i18n_trans_</xsl:text><xsl:value-of select="$lang"/><xsl:text>).
-export([get/1]).

%% @doc Get text with `ID' in "</xsl:text>
    <xsl:value-of select="$lang"/><xsl:text>" language
</xsl:text>

<xsl:variable name="default-lang" select="/translations/@default-lang"/>

<xsl:variable name="max-id-len">
    <xsl:call-template name="maximum">
        <xsl:with-param name="values" select="lang[@iso = $lang]/text/@id"/>
    </xsl:call-template>
</xsl:variable>

<xsl:for-each select="lang[@iso = $lang]">
    <xsl:for-each select="text">
        <xsl:variable name="id" select="@id"/>
        <xsl:variable name="default-lang-id-type"
            select="/translations/lang[@iso = $default-lang]/text[@id = $id]/@type"/>
        <xsl:if test="@type = 'static' or $default-lang-id-type = 'static'">
            <xsl:variable name="title">
                <xsl:choose>
                    <xsl:when test="@value"><xsl:value-of select="@value"/></xsl:when>
                    <xsl:otherwise><xsl:value-of select="self::node()/text()"/></xsl:otherwise>
                </xsl:choose>
            </xsl:variable>

            <xsl:text>get(</xsl:text>
            <xsl:call-template name="pad">
                <xsl:with-param name="padCount">
                    <xsl:value-of select="$max-id-len - string-length(@id)"/>
                </xsl:with-param>
            </xsl:call-template>
            <xsl:value-of select="@id"/>
            <xsl:text>) -&gt; &lt;&lt;"</xsl:text>
            <xsl:value-of select="$title"/>
            <xsl:text>">>;&#10;</xsl:text>
        </xsl:if>
        <xsl:if test="@type and not(@type = 'dynamic') and not(@type = 'static')">
            <xsl:message terminate="yes">
                <xsl:text>Invalid value of /translations/lang[</xsl:text>
                <xsl:value-of select="@iso"/>
                <xsl:text>]/id[</xsl:text>
                <xsl:value-of select="@id"/>
                <xsl:text>]/type = '</xsl:text>
                <xsl:value-of select="@type"/>
                <xsl:text>'</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:if test="$default-lang-id-type
                        and not($default-lang-id-type = 'dynamic')
                        and not($default-lang-id-type = 'static')">
            <xsl:message terminate="yes">
                <xsl:text>Invalid value of /translations/lang[</xsl:text>
                <xsl:value-of select="$default-lang"/>
                <xsl:text>]/id[</xsl:text>
                <xsl:value-of select="@id"/>
                <xsl:text>]/type = '</xsl:text>
                <xsl:value-of select="@type"/>
                <xsl:text>'</xsl:text>
            </xsl:message>
        </xsl:if>
    </xsl:for-each>
</xsl:for-each>
<xsl:text>get(</xsl:text>
<xsl:call-template name="pad">
    <xsl:with-param name="padCount">
        <xsl:value-of select="$max-id-len - 1"/>
    </xsl:with-param>
</xsl:call-template>
<xsl:text>I) -&gt;
    try
        i18n_trans_server:get(</xsl:text><xsl:value-of select="$lang"/><xsl:text>, I)
    catch
        _:{noproc,_} ->
            throw(no_server_proc);
        _:_ ->
            throw({not_found, I})
    end.

</xsl:text>
</xsl:template>

<xsl:template name="format-const-name">
    <xsl:param name="text"/>
    <xsl:param name="width"/>
    
    <xsl:variable name="smallcase" select="'abcdefghijklmnopqrstuvwxyz'" />
    <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

    <xsl:text>TXT_</xsl:text>
    <xsl:value-of select="
        translate(
            translate(
                translate(substring($text, 1, 40), $smallcase, $uppercase),
                '&amp;@#?!$()[]{}&quot;', ''),
            '- ', '__')
    "/>
</xsl:template>

</xsl:stylesheet>
