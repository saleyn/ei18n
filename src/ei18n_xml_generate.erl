%%%----------------------------------------------------------------------------
%%% @doc I18n code generator.
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2012 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2012-10-07
%%%----------------------------------------------------------------------------
-module(ei18n_xml_generate).
-author('saleyn@gmail.com').

%% API
-export([check_files/2, generate/4, dynamic_data/1, save_dynamic_data/2]).

-include_lib("xmerl/include/xmerl.hrl").

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @spec (XmlFile::string(), NowDateTime::tuple()) -> [Filename::string()].
%% @doc Check which internationalization modules need to be recreated
%% @end
%%-----------------------------------------------------------------------------
-spec check_files(string(), calendar:datetime()) -> [string()].
check_files(XmlFile, LastModifiedDate) ->
    Languages = list_languages(XmlFile),
    lists:sort(lists:foldl(
        fun(F, A) ->
            case filelib:last_modified(F) > LastModifiedDate of
            true  -> A;
            false -> [F | A]
            end
        end,
        [],
        [filename:join("include", "ei18n.hrl"),
         filename:join("src", "ei18n.erl")
         | [filename:join("src", lang_module_name(L) ++ ".erl") || L <- Languages]])).

%%-----------------------------------------------------------------------------
%% @spec (OutDir::string(), XmlFile::string(),
%%              Author::string(), Email::string()) -> ok
%% @doc Generate internationalization modules from an XML specification file.
%%      `OutDir' is the destination directory. If that directory containst
%%      `"include"' and `"src"' subdirectories, the corresponding header and
%%      source files will be created there, otherwise they will be created in
%%      `OutDir'.  `XmlFile' is the input file containing internationalization
%%      text.  `Author' is the author generating the files.  `Email' is the
%%      email of the `Author'.  Sample format of the XML message file is in
%%      `"spec/messages.xml"'.
%% @end
%%-----------------------------------------------------------------------------
-spec generate(string(), string(), string(), string()) -> ok.
generate(OutDir, XmlFile, Author, Email)
  when is_list(OutDir), is_list(XmlFile), is_list(Author), is_list(Email) ->
    Doc = scan_file(XmlFile),
    HeaderBin = header_data(XmlFile, Author, Email, Doc),
    I18nBin   = i18n_data(XmlFile, Author, Email, Doc),
    I18nLangs = [{Lang, i18n_lang_data(XmlFile, Lang, Author, Email, Doc)}
                    || Lang <- languages(Doc)],
    PrivDir = filename:join(OutDir, "include"),
    case filelib:is_dir(PrivDir) of
    true  -> HDir = PrivDir;
    false -> HDir = OutDir
    end,
    SrcDir = filename:join(OutDir, "src"),
    case filelib:is_dir(SrcDir) of
    true  -> SDir = SrcDir;
    false -> SDir = OutDir
    end,

    ok = file:write_file(filename:join(HDir, "ei18n.hrl"), HeaderBin),
    ok = file:write_file(filename:join(SDir, "ei18n.erl"), I18nBin),
    lists:foreach(fun({Lang, Bin}) ->
        ok = file:write_file(filename:join(SDir, lang_module_name(Lang) ++ ".erl"), Bin)
    end, I18nLangs).

%%-----------------------------------------------------------------------------
%% @spec (XmlFile::string()) ->
%%           [{{Key::binary(), Lang::atom()}, Value::binary()}].
%% @doc Generate a list of dynmic lookup translation entries from an XML
%%      specification file. The result of this call can be loaded to the
%%      ei18n_trans_server using `ei18n_trans_server:reload/1' function.
%% @end
%%-----------------------------------------------------------------------------
-spec dynamic_data(string()) -> [{{binary(), atom()}, binary()}].
dynamic_data(XmlFile) ->
    Doc = scan_file(XmlFile),
    DefLangS = default_lang(Doc),
    DefTree  = name_types_tree(DefLangS, undefined, Doc),
    DefLang  = list_to_atom(DefLangS),
    AllLang  = languages(Doc),

    lists:foldl(fun(Lang, Acc) ->
        Tree = name_types_tree(Lang, "static", Doc),
        accumulate(gb_trees:next(gb_trees:iterator(Tree)),
            list_to_atom(Lang), DefLang, DefTree, Acc)
    end, [], AllLang).
 
%%-----------------------------------------------------------------------------
%% @spec (OutFile::string(), XmlFile::string()) ->
%%           [{{Key::binary(), Lang::atom()}, Value::binary()}].
%% @doc Save dynmic lookup translation entries from an XML
%%      specification file to a binary file, so that it later could be loaded
%%      to the ei18n_trans_server.
%% @end
%%-----------------------------------------------------------------------------
-spec save_dynamic_data(string(), string()) -> [{{binary(), atom()}, binary()}].
save_dynamic_data(OutFile, XmlFile) ->
    List = dynamic_data(XmlFile),
    file:write_file(OutFile, term_to_binary(List)).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

accumulate(none, _Lang, _DefLang, _DefTree, Acc) ->
    Acc;
accumulate({K, {_Tp, V}, It}, DefLang, DefLang, DefTree, Acc) ->
    accumulate(gb_trees:next(It), DefLang, DefLang, DefTree,
        [{{to_key(K), DefLang}, unicode:characters_to_binary(V, unicode)} | Acc]);
accumulate({K, {_Tp, V}, It}, Lang, DefLang, DefTree, Acc) ->
    case gb_trees:lookup(K, DefTree) of
    {value, {static, _}} ->
        accumulate(gb_trees:next(It), Lang, DefLang, DefTree, Acc);
    {value, _} ->
        accumulate(gb_trees:next(It), Lang, DefLang, DefTree,
            [{{to_key(K), Lang}, unicode:characters_to_binary(V, unicode)} | Acc]);
    none ->
        throw({default_name_not_found, Lang, K})
    end.

define_prefix() -> "I18N_".

lang_module_name(Suffix) when is_list(Suffix) ->
    "ei18n_" ++ Suffix.

list_languages(XmlFile) ->
    case os:type() of
    {win32, _} ->
        Doc = scan_file(XmlFile),
        languages(Doc);
    {unix, _} ->
        case filelib:is_file(XmlFile) of
        true ->
            lists:nthtail(10, string:tokens(
                os:cmd("egrep -o \"lang *iso=\\\"([^\\\"]*)\" \"" ++ XmlFile ++ "\""),
                       "\n"));
        false ->
            throw({file_has_no_languages, XmlFile})
        end
    end.

languages(Doc) ->
    [V || #xmlAttribute{value=V} <- xmerl_xpath:string("/translations/lang/@iso", Doc)].

version(Doc) ->
    [#xmlAttribute{value=V}] = xmerl_xpath:string("/translations/@version", Doc),
    V.

header_data(XmlFile, Username, Email, Doc) ->
  <<"%%%----------------------------------------------------------------------------\n"
    "%%% Internationalization text constants\n"
    "%%%\n"
    "%%% Copyright (c) 2012 Serge Aleynikov <saleyn@gmail.com>\n"
    "%%%----------------------------------------------------------------------------\n",
    (version_by_created(XmlFile, Username, Email, Doc))/binary,
    "\n",
    (print_defines(Doc))/binary>>.

i18n_data(XmlFile, Username, Email, Doc) ->
  <<"%%%----------------------------------------------------------------------------\n"
    "%%% @doc Internationalization i18n API.\n"
    "%%%\n"
    "%%% @copyright 2012 Serge Aleynikov <saleyn@gmail.com>\n"
    "%%% @end\n"
    "%%%----------------------------------------------------------------------------\n",
    (version_by_created(XmlFile, Username, Email, Doc))/binary,
    "-module(ei18n).\n"
    "-export([default_language/0, supports/1, info/1, get/1, get/2, reload/0]).\n"
    "\n"
    "%% @doc Returns default system language\n"
    "-spec default_language() -> atom().\n"
    "default_language() -> ", (list_to_binary(default_lang(Doc)))/binary, ".\n\n"
    "%% @doc Returns `true' if given language is supported.\n"
    "-spec supports(atom()) -> boolean().\n",
    (list_to_binary(
        [["supports(", I, ") -> true;\n"] || I <- languages(Doc)]))/binary,
    "supports(_)  -> false.\n"
    "\n"
    "%% @doc Returns a list of supported languages or language modules.\n"
    "-spec info(languages | modules) -> [atom()].\n"
    "info(languages) ->\n"
    "    [ ",
    (list_to_binary(string:join([L++"\n" || L <- languages(Doc)], "    , ")))/binary,
    "    ];\n"
    "info(modules) ->\n"
    "    [lang_module(L) || L <- info(languages)].\n\n"
    "%% @doc Get translation string associated with `ID' in the default language\n"
    "-spec get(integer()) -> binary().\n"
    "get(ID) when is_integer(ID) ->\n"
    "    (lang_module(default_language())):get(ID).\n"
    "\n"
    "%% @doc Get translation string associated with `ID' in the given language\n"
    "-spec get(atom(), integer()) -> binary().\n"
    "get(ID, Lang) when is_integer(ID), is_atom(Lang) ->\n"
    "    (lang_module(Lang)):get(ID).\n"
    "\n"
    "%% @doc Reload modified language modules\n"
    "-spec reload() -> [{atom(), any()}].\n"
    "reload() ->\n"
    "    Modified = [M || M <- [?MODULE | info(modules)], module_modified(M)],\n"
    "    [c:l(M) || M <- Modified].\n"
    "\n"
    "%% Internal functions\n",
    (list_to_binary([["lang_module(", L, ") -> ", lang_module_name(L), ";\n"]
                        || L <- languages(Doc)]))/binary,
    "lang_module(_)  -> ",
        (list_to_binary(lang_module_name(default_lang(Doc))))/binary, ".\n",
    "module_modified(Module) ->\n"
    "    case code:is_loaded(Module) of\n"
    "    {file, preloaded} ->\n"
    "        false;\n"
    "    {file, Path} ->\n"
    "        CompileOpts = proplists:get_value(compile, Module:module_info()),\n"
    "        CompileTime = proplists:get_value(time, CompileOpts),\n"
    "        Src = proplists:get_value(source, CompileOpts),\n"
    "        module_modified(Path, CompileTime, Src);\n"
    "    _ ->\n"
    "        false\n"
    "    end.\n"
    "\n\n"
    "module_modified(Path, PrevCompileTime, PrevSrc) ->\n"
    "    case find_module_file(Path) of\n"
    "    false ->\n"
    "        false;\n"
    "    ModPath ->\n"
    "        {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, [\"CInf\"]),\n"
    "        CompileOpts =  binary_to_term(CB),\n"
    "        CompileTime = proplists:get_value(time, CompileOpts),\n"
    "        Src = proplists:get_value(source, CompileOpts),\n"
    "        not (CompileTime == PrevCompileTime) and (Src == PrevSrc)\n"
    "    end.\n"
    "\n"
    "find_module_file(Path) ->\n"
    "    case file:read_file_info(Path) of\n"
    "    {ok, _} ->\n"
    "        Path;\n"
    "    _ ->\n"
    "        %% maybe the path was changed?\n"
    "        case code:where_is_file(filename:basename(Path)) of\n"
    "        non_existing ->\n"
    "            false;\n"
    "        NewPath ->\n"
    "            NewPath\n"
    "        end\n"
    "    end.">>.

i18n_lang_data(XmlFile, Lang, Username, Email, Doc) ->
  <<"%%%----------------------------------------------------------------------------\n"
    "%%% @doc Internationalization text constants for \"",
        (list_to_binary(Lang))/binary, "\" language.\n"
    "%%%\n"
    "%%% @copyright 2012 Serge Aleynikov <saleyn@gmail.com>\n"
    "%%% @end\n",
    (version_by_created(XmlFile, Username, Email, Doc))/binary,
    "-module(", (list_to_binary(lang_module_name(Lang)))/binary, ").\n"
    "-export([get/1]).\n\n"
    "-include(\"ei18n.hrl\").\n\n"
    "%% @doc Get text with `ID' in `", (list_to_binary(Lang))/binary, "' language\n",
    (list_to_binary(lang_data(Lang, Doc)))/binary,
    "get(I) ->\n"
    "   try\n"
    "       ei18n_trans_server:get(I, ", (list_to_binary(Lang))/binary, ")\n"
    "   catch\n"
    "       _:{noproc,_} ->\n"
    "           throw(no_server_proc);\n"
    "       _:_ ->\n"
    "           throw({not_found, I})\n"
    "   end.\n">>.

lang_data(Lang, Doc) ->
    DefLang = default_lang(Doc),
    Tree    = name_types_tree(DefLang, undefined, Doc),
    List = lists:foldl(fun(#xmlElement{attributes = Attrs, content = Content}, Acc) ->
        {Name, Val} = to_name_val(Lang, Attrs, Content),
        case def_lang_text_type(Lang, Name, Tree) of
        static  -> [{Name, Val} | Acc];
        dynamic -> Acc
        end 
    end, [], xmerl_xpath:string("/translations/lang[@iso = '" ++ Lang ++ "']/text", Doc)),
    MaxLen = lists:max([length(element(1, V)) || V <- List]),
    [["get(?I18N_", string:left(N, MaxLen, $ ), ") -> <<\"", V, "\">>;\n"]
        || {N,V} <- lists:sort(List)].


scan_file(XmlFile) ->
    case xmerl_scan:file(XmlFile) of
    {#xmlElement{} = Doc, _} ->
        Doc;
    {error, Reason} ->
        throw(Reason)
    end.

name_types_tree(Lang, ExcludeType, Doc) ->
    lists:foldl(
        fun(#xmlElement{attributes = A, content = C}, T) ->
            {Name, Val} = to_name_val(Lang,A,C),
            gb_trees:insert(Name, {to_type(get_attr(type, A, dynamic)), Val}, T)
        end,
        gb_trees:empty(),
        xmerl_xpath:string("/translations/lang[@iso = '" ++ Lang ++ "']/text" ++
            case ExcludeType of
            undefined -> [];
            _         -> "[@type != '" ++ ExcludeType ++ "']"
            end, Doc)).

to_name_val(Lang, Attrs, Content) ->
    case Content of
    [#xmlText{value = Value}|_] -> ok;
    [] -> Value = get_attr(value, Attrs, undefined)
    end,
    case get_attr(name, Attrs, undefined) of
    undefined when Value =:= undefined ->
        throw({undefined_name, Lang, Attrs});
    undefined ->
        {format_const_name(string:substr(Value, 1, 40)),
         unicode:characters_to_binary(Value, unicode)};
    Other ->
        {format_const_name(Other), unicode:characters_to_binary(Value, unicode)}
    end.
    
def_lang_text_type(Lang, Name, Tree) ->
    case gb_trees:lookup(Name, Tree) of
    {value, {Tp,_}} -> Tp;
    none        -> throw({no_name_in_default_language, Lang, Name})
    end.

default_lang(Doc) ->
    [#xmlAttribute{value=L}] = xmerl_xpath:string("/translations/@default-lang", Doc),
    L.

version_by_created(XmlFile, Username, Email, Doc) ->
  <<"%%%      Version: ", (list_to_binary(version(Doc)))/binary, "\n"
    "%%% Generated by: ", (list_to_binary(Username ++ " " ++
            if Email =:= [] orelse Email =:= undefined ->
                [];
            true ->
                "<" ++ Email ++ ">"
            end))/binary, "\n"
    "%%%      Created: ", (now_time())/binary, "\n"
    "%%%----------------------------------------------------------------------------\n"
    "%%%\n"
    "%%% This file is automatically generated by ",
        (atom_to_binary(?MODULE, latin1))/binary, "\n"
    "%%% from ", (list_to_binary(filename:basename(XmlFile)))/binary, "\n"
    "%%%\n"
    "%%%            >>>>>>>  !!! DO NOT MODIFY BY HAND  !!! <<<<<<<\n"
    "%%%\n"
    "%%%----------------------------------------------------------------------------\n">>.

format_const_name(Name) ->
    N = re:replace(string:to_upper(Name), "[&@?!()\'\"\\[\\]{}]", "",[{return,list},global]),
    re:replace(N, "[- ][- ]*", "_", [{return, list}, global]).

print_defines(Doc) ->
    Defines = [{format_const_name(get_attr(name, Attrs, undefined)), integer_to_list(I)}
        || #xmlElement{pos = I, attributes = Attrs} <- xmerl_xpath:string(
            "lang[@iso = /translations/@default-lang]/text[@type = 'static']", Doc)],
    PrefLen = length(define_prefix()),
    MaxLen  = lists:max([length(N)+PrefLen || {N,_} <- Defines]),
    MaxVLen = lists:max([length(N) || {_,N} <- Defines]),
    list_to_binary([
        ["-define(", string:left(define_prefix() ++ I, MaxLen, $ ), ", ",
         string:right(V, MaxVLen, $ ), ").\n"]
        || {I,V} <- Defines
    ]).

get_attr(Name, Attrs, Default) ->
    case lists:keyfind(Name, #xmlAttribute.name, Attrs) of
    #xmlAttribute{value = N} -> N;
    false -> Default
    end.

now_time() ->
    {{Y,Mo,D},{H,M,S}} = calendar:local_time(),
    list_to_binary(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
        [Y,Mo,D,H,M,S])).

to_type("static")  -> static;
to_type("dynamic") -> dynamic;
to_type(dynamic)   -> dynamic.

to_key([I | _] = K) when I >= $1, I =< $9   -> list_to_integer(K);
to_key(K)                                   -> list_to_binary(K).
