%% -----------------------------------------------------------------------------
%% Copyright (c) 2012 Serge Aleynikov <saleyn@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(ei18n_rebar_plugin).
-export([pre_compile/2, clean/2]).

pre_compile(Config, _) ->
    rebar_log:log(debug, "plugin working in ~s\n", [rebar_utils:get_cwd()]),
    F =
        case rebar_config:get_all(Config, ei18n_opts) of
        undefined ->
            throw("Undefined ei18n_opts options!");
        Opts when is_list(Opts) ->
            proplists:get_value(xml_file, Opts, "priv/i18n.xml")
        end,

    Dir = rebar_config:get_xconf(Config, base_dir),
    XmlFile = xml_file_name(Dir, F, element(1, os:type())),

    rebar_log:log(debug, "ei18n XML specifications file: ~s\n", [XmlFile]),

    filelib:is_file(XmlFile) orelse
        throw("I18n XML specification file not found!"),

    case rebar_utils:processing_base_dir(Config) of
    true ->
        Time  = filelib:last_modified(XmlFile),
        case ei18n_xml_generate:check_files(XmlFile, Time) of
        [] -> ok;
        _ ->
            {ok, User}  = git_config_get("user.name"),
            {ok, Email} = git_config_get("user.email"),
            ei18n_xml_generate:generate(".", XmlFile, User, Email),
            ok
        end;
    false ->
        rebar_log:log(debug, "Not base_dir\n", []),
        ok
    end.

clean(Config, _) ->
    case rebar_utils:processing_base_dir(Config) of
        true ->
            file:delete("src/ei18n.erl"),
            [file:delete(F) || F <- filelib:wildcard("src/ei18n_??.erl")],
            file:delete("include/ei18n.hrl"),
            ok;
        false ->
            ok
    end.

git_config_get(Opt) ->
    Opts = [{use_stdout, false}, return_on_error],
    case rebar_utils:sh("git config --get " ++ Opt, Opts) of
    {ok, V} ->
        {ok, string:substr(V, 1, length(V)-1)};
    Error ->
        Error
    end.

xml_file_name(_Dir, [_, $: | _] = File, win32) ->
    File;
xml_file_name(_Dir, [$\\ | _] = File, win32) ->
    File;
xml_file_name(Dir, File, win32) ->
    filename:join(Dir, File);
xml_file_name(_Dir, [$/ | _] = File, unix) ->
    File;
xml_file_name(Dir, File, unix) ->
    filename:join(Dir, File).
