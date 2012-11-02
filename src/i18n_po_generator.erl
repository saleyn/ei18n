%%%----------------------------------------------------------------------------
%%% @doc Generates I18n po files
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2012 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2012-10-07
%%%----------------------------------------------------------------------------
-module(i18n_po_generator).
-author('saleyn@gmail.com').

%% API
-export([write_file/3]).

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

-spec write_file(string(), [{string(), string()}], [{string(), string()}]) -> ok.
write_file(Filename, Items, Fuzzy) ->
    {ok, Fd} = file:open(Filename, [write, {encoding, unicode}, binary, raw]),
	write_header(Fd),
    write_entries(Fd, Items),
	write_fuzzy_entries(Fd, Fuzzy), 
    file:close(Fd).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

write_entries(Fd, Items)->
    lists:foreach(fun({Id,Translation}) ->
		file:write(Fd, <<"msgid \"\n">>),
		write_string(Fd, Id),
		file:write(Fd, <<"\"\nmsgstr \"">>),
		write_string(Fd, Translation),
        file:write(Fd, <<"\"\n">>)
	end, Items).

write_string(Fd, S) ->
    file:write(Fd,
        unicode:characters_to_binary(
            lists:foldr(fun
                ($",  Acc) -> [$\\,$"|Acc];
                ($\\, Acc) -> [$\\,$\\|Acc];
                ($\n, Acc) -> [$\\,$n|Acc];
                (C,   Acc) -> [C|Acc] 
            end, [], S),
            unicode)).

write_fuzzy_entries(_Fd, []) ->
    ok;
write_fuzzy_entries(Fd, Items) ->
	file:write(Fd, <<"\n">>),
	lists:foreach(fun({Id,Translation}) ->
        file:write(Fd, <<"#, fuzzy\n">>),
		file:write(Fd, <<"msgid \"">>),
		write_string(Fd, Id),
		file:write(Fd, <<"\"\nmsgstr \"">>),
		write_string(Fd, Translation),
		file:write(Fd, "\"\n")
	end, Items).

write_header(Fd) ->
    file:write_file(Fd,
  <<"# SOME DESCRIPTIVE TITLE.\n"
    "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER\n"
    "# This file is distributed under the same license as the PACKAGE package.\n"
    "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n"
    "#\n"
    "# NB: Consider using poEdit <http://poedit.sourceforge.net>\n"
    "#\n"
    "#\n"
    "#, fuzzy\n"
    "msgid \"\"\n"
    "msgstr \"\"\n"
    "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
    "\"POT-Creation-Date: 2003-10-21 16:45+0200\\n\"\n"
    "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n"
    "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n"
    "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n"
    "\"Language: LC\\n\""
    "\"MIME-Version: 1.0\\n\"\n"
    "\"Content-Type: text/plain; charset=iso-8859-1\\n\"\n"
    "\"Content-Transfer-Encoding: 8bit\\n\"\n">>).

