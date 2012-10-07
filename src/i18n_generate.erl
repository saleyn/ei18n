%%%----------------------------------------------------------------------------
%%% @doc I18n code generator.
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2012 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2012-10-07
%%%----------------------------------------------------------------------------
-module(i18n_generate).
-author('saleyn@gmail.com').

%% API
-export([generate/1]).

-include_lib("xmerl/include/xmerl.hrl").

-record(item, {
      id        :: integer() | atom()
    , type      :: static | dynamic
    , value     :: binary()
}).

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------
generate(XmlFile) ->
    {#xmlElement{name = translations, attributes = Attr, content = Content},_} =
        xmerl_scan:file(XmlFile, [{space,normalize}]),
    #xmlAttribute{value = DefLang} =
        lists:keyfind('default-lang', #xmlAttribute.name, Attr),
    #xmlAttribute{value = Version} =
        lists:keyfind(version, #xmlAttribute.name, Attr),
    %% Get a list of all language nodes

    % DefLangText = xmerl_xpath:string("/translations/lang[@iso = 'en']/text", Doc),
    
    Languages = [X || #xmlElement{name = lang} = X <- Content],
    %% Get the text nodes for the default language
    [DefLangNode | _] = lists:dropwhile(
        fun(#xmlElement{attributes = Attrs}) ->
            lists:keyfind(DefLang, #xmlAttribute.value, Attrs) =:= false
        end, Languages),
    DefLangTexts = [blob(DefLang, I) || #xmlElement{} = I <- DefLangNode#xmlElement.content],
    {DefLang, Version, DefLangTexts}.

%%-----------------------------------------------------------------------------
%% @spec () ->
%% @doc 
%% @end
%%-----------------------------------------------------------------------------

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

blob(Lang, #xmlElement{attributes = Attrs, content = Content}) ->
    #xmlAttribute{value = ID} = lists:keyfind(id, #xmlAttribute.name, Attrs),
    case lists:keyfind(type, #xmlAttribute.name, Attrs) of
    #xmlAttribute{value = Type} ->
        ok;
    false ->
        Type = undefined
    end,
    case lists:keyfind(value, #xmlAttribute.name, Attrs) of
    #xmlAttribute{value = Text} ->
        #item{id=to_id(ID), type=to_type(Type), value=list_to_binary(Text)};
    false when Content =/= [] ->
        #xmlText{value = Text} = hd(Content),
        #item{id=to_id(ID), type=to_type(Type), value=list_to_binary(Text)};
    false ->
        throw({no_text_found_in_node_id, Lang, ID})
    end.

to_id(I) ->
    try_to_int(I).

try_to_int([I|_] = L) when $0 =< I, I =< $9 ->
    list_to_integer(L);
try_to_int(Other) ->
    list_to_atom(Other).

to_type("static")  -> static;
to_type("dynamic") -> dynamic;
to_type(undefined) -> undefined.
