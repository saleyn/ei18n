<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" media-type="text/plain" encoding="utf-8"/>

<xsl:param name="now"/>
<xsl:param name="xslfile"/>
<xsl:param name="xmlfile"/>
<xsl:param name="user"/>
<xsl:param name="email"/>

<xsl:template match="/translations">
<xsl:text>%%%----------------------------------------------------------------------------
%%% @doc Internationalization i18n API.
%%%
%%% @copyright 2012 Serge Aleynikov &lt;saleyn@gmail.com>
%%% @end
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
-module(i18n).
-export([default_language/0, supports/1, info/1, get/1, get/2, reload/0]).

</xsl:text>

<xsl:variable name="max-title-len" select="40"/>
<xsl:variable name="default-lang" select="@default-lang"/>

<xsl:text>%% @doc Returns default system language
-spec default_language() -> atom().
default_language() -&gt; </xsl:text>
<xsl:value-of select="@default-lang"/><xsl:text>.

%% @doc Returns `true' if given language is supported.
-spec supports(atom()) -> boolean().
</xsl:text>

<xsl:for-each select="lang">
    <xsl:text>supports(</xsl:text>
    <xsl:value-of select="@iso"/>
    <xsl:text>) -&gt; true;&#10;</xsl:text>
</xsl:for-each>
<xsl:text>supports(_)  -&gt; false.

%% @doc Returns a list of supported languages or language modules.
-spec info(languages | modules) -> [atom()].
info(languages) ->
    [ </xsl:text>
<xsl:for-each select="lang">
    <xsl:if test="position() > 1"><xsl:text>    , </xsl:text></xsl:if>
    <xsl:value-of select="@iso"/>
    <xsl:text>&#10;</xsl:text>
</xsl:for-each>
<xsl:text>    ];
info(modules) ->
    [lang_module(L) || L &lt;- info(languages)].

%% @doc Get translation string associated with `ID' in the default language
-spec get(integer()) -> binary().
get(ID) when is_integer(ID) -&gt;
    (lang_module(default_language())):get(ID).

%% @doc Get translation string associated with `ID' in the given language
-spec get(atom(), integer()) -> binary().
get(ID, Lang) when is_integer(ID), is_atom(Lang) ->
    (lang_module(Lang)):get(ID).

%% @doc Reload modified language modules
-spec reload() -> [{atom(), any()}].
reload() ->
    Modified = [M || M &lt;- [?MODULE | info(modules)], module_modified(M)],
    [c:l(M) || M &lt;- Modified].

%% Internal functions
</xsl:text>

<xsl:for-each select="lang">
    <xsl:text>lang_module(</xsl:text>
    <xsl:value-of select="@iso"/>
    <xsl:text>) -&gt; i18n_trans_</xsl:text>
    <xsl:value-of select="@iso"/>
    <xsl:text>;
</xsl:text>
</xsl:for-each>
<xsl:text>lang_module(_)  -&gt; i18n_trans_</xsl:text>
<xsl:value-of select="$default-lang"/><xsl:text>.

module_modified(Module) ->
    case code:is_loaded(Module) of
    {file, preloaded} ->
        false;
    {file, Path} ->
        CompileOpts = proplists:get_value(compile, Module:module_info()),
        CompileTime = proplists:get_value(time, CompileOpts),
        Src = proplists:get_value(source, CompileOpts),
        module_modified(Path, CompileTime, Src);
    _ ->
        false
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
    false ->
        false;
    ModPath ->
        {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
        CompileOpts =  binary_to_term(CB),
        CompileTime = proplists:get_value(time, CompileOpts),
        Src = proplists:get_value(source, CompileOpts),
        not (CompileTime == PrevCompileTime) and (Src == PrevSrc)
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
    {ok, _} ->
        Path;
    _ ->
        %% maybe the path was changed?
        case code:where_is_file(filename:basename(Path)) of
        non_existing ->
            false;
        NewPath ->
            NewPath
        end
    end.

</xsl:text>

</xsl:template>

</xsl:stylesheet>
