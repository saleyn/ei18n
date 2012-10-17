%%%----------------------------------------------------------------------------
%%% @doc Translation server of i18n internationalization framework.
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2012 Serge Aleynikov
%%%
%%% @type server_options() = {reload_check_sec, Timeout::integer()}
%%%                        | {reload_impl, Module::atom()}.
%%%         Server configuration options.
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2012-10-07
%%%----------------------------------------------------------------------------
-module(i18n_trans_server).
-author('saleyn@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/1, start/1, get/1, get/2, update/3, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Internal default API of data reloading implementation
-export([impl_init/2, impl_reload/2, impl_stop/1]).

-record(state, {
      tref      :: reference()  % Reference of reload checking timer
    , timeout   :: integer()    % Reload checking timer timeout
    , impl      :: atom()       % Reload implementation module
    , impl_state                % Opaque implemenation module's state
}).

-type server_options() :: [ {reload_check_sec, integer()}
                          | {reload_impl, atom()}].

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @spec (Options) -> {ok, Pid} | ignore | {error, Reason} 
%%          Options = server_options()
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns `{error,Reason}'.
%%      If init/1 returns `{stop,Reason}' or ignore, the process is
%%      terminated and the function returns `{error,Reason}' or ignore,
%%      respectively.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(server_options()) -> pid().
start_link(Options) when is_list(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%-----------------------------------------------------------------------------
%% @spec (Options) -> {ok, Pid} | {error, Reason} 
%%          Options = server_options()
%% @doc Start the server outside of supervision tree.
%% @end
%%-----------------------------------------------------------------------------
-spec start(server_options()) -> pid().
start(Options) when is_list(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

%%-----------------------------------------------------------------------------
%% @spec (ID) -> binary()
%%          ID = integer() | atom()
%% @doc Get a text blob in the default language associated with `ID'. On error
%%      it throws a `Reason'.
%% @end
%%-----------------------------------------------------------------------------
-spec get(integer() | atom()) -> binary() | {error, any()}.
get(ID) when is_integer(ID); is_atom(ID) ->
    get(i18n:default_language(), ID).

%%-----------------------------------------------------------------------------
%% @spec (Lang, ID) -> binary()
%%          ID = integer() | atom()
%% @doc Get a text blob in the `Lang' language associated with `ID'. On error
%%      it throws a `Reason'.
%% @end
%%-----------------------------------------------------------------------------
-spec get(atom(), integer() | atom()) -> binary() | not_found.
get(Lang, ID) when is_atom(Lang) ->
    case gen_server:call(?MODULE, {get, {Lang, ID}}, infinity) of
    B when is_binary(B) ->
        B;
    {error, Reason} ->
        throw(Reason)
    end.

-spec update(atom(), integer() | atom(), binary()) -> ok.
update(Lang, ID, Value) when is_atom(Lang) andalso is_binary(Value)
        andalso (is_integer(ID) orelse is_atom(ID)) -> 
    gen_server:call(?MODULE, {update, {Lang, ID}, Value}).

-spec delete(atom(), integer() | atom()) -> ok.
delete(Lang, ID) when is_atom(Lang)
        andalso (is_integer(ID) orelse is_atom(ID)) ->
    gen_server:call(?MODULE, {delete, {Lang, ID}}).

%%%----------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Args) -> {ok, State} | {ok, State, Timeout} |
%%                 ignore | {stop, Reason}
%% @doc Initiates the server
%% @end
%%-----------------------------------------------------------------------------
init(Options) ->
    process_flag(trap_exit, true),
    try
        Timeout = proplists:get_value(reload_check_sec, Options, 5) * 1000,
        RelMod  = proplists:get_value(reload_impl, Options, ?MODULE),

        ets:new(?MODULE,
            [protected, named_table, {read_concurrency,true}]),

        IState = RelMod:impl_init(self(), Options),

        Ref = erlang:send_after(Timeout, self(), check_and_reload),
        {ok, #state{tref = Ref, timeout = Timeout,
                    impl = RelMod, impl_state=IState}}
    catch _:What ->
        {stop, What}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Request, From, State) ->
%%            {reply, Reply, State} |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State} |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, Reply, State} |
%%            {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%-----------------------------------------------------------------------------
handle_call({get, Key}, _From, State) ->
    case lookup(Key) of
    V when is_binary(V) ->
        {reply, V, State};
    false ->
        DefLang = i18n:default_language(),
        case Key of
        {Lang, _} when Lang =:= DefLang ->
            {reply, not_found, State};
        {_, ID} ->
            case lookup({DefLang, ID}) of
            V2 when is_binary(V2) ->
                {reply, V2, State};
            false ->
                {reply, not_found, State}
            end
        end
    end;

handle_call({update, Key, Value}, _From, State) ->
    true = ets:insert(?MODULE, {Key, Value}),
    {reply, ok, State};

handle_call({delete, {_, _} = Key}, _From, State) ->
    true = ets:delete(?MODULE, Key),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Msg, State) -> {noreply, State} |
%%                       {noreply, State, Timeout} |
%%                       {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%-----------------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Info, State) -> {noreply, State} |
%%                        {noreply, State, Timeout} |
%%                        {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%-----------------------------------------------------------------------------
handle_info(check_and_reload, #state{impl = Module, impl_state = IState} = State) ->
    NewIState = Module:impl_reload(self(), IState),
    NewS = State#state{impl_state = NewIState},
    Ref = erlang:send_after(NewS#state.timeout, self(), check_and_reload),
    {noreply, NewS#state{tref = Ref}};

handle_info(_Info, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%-----------------------------------------------------------------------------
terminate(_Reason, #state{impl = Module, impl_state = IState}) ->
    catch Module:impl_stop(IState),
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

lookup(Key) ->
    case ets:lookup(?MODULE, Key) of
    [{_,V}] -> V;
    []      -> false
    end.


impl_init(_ServerPid, _Options) ->
    undefined.

impl_reload(_ServerPid, State) ->
    State.

impl_stop(_State) ->
    ok.
