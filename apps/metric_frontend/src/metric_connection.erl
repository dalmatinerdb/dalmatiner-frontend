%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(metric_connection).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1, get/3, list/0]).
-ignore_xref([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, metrics=[], last_read = {0, 0, 0}, host, port}).

%%%===================================================================
%%% API
%%%===================================================================

get(Metric, Time, Count) ->
    poolboy:transaction(backend_connection,
                        fun(Worker) ->
                                gen_server:call(Worker, {get, Metric, Time, Count})
                        end).

list() ->
    poolboy:transaction(backend_connection,
                        fun(Worker) ->
                                gen_server:call(Worker, list)
                        end).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Host, Port]) ->
    {ok, S} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    {ok, #state{socket = S, host=Host, port=Port}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, Metric, Time, Count}, _From, State = #state{socket = S}) ->
    L = byte_size(Metric),
    Msg = <<2, L:16/integer, Metric:L/binary, Time:64/integer, Count:32/integer>>,
    ok = gen_tcp:send(S, Msg),
    Read = Count*9,
    case gen_tcp:recv(S, Read, 3000) of
        {ok, D} ->
            {reply, {ok, D}, State};
        {error, _} ->
            gen_tcp:close(S),
            {ok, S1} = gen_tcp:connect(State#state.host, State#state.port,
                                       [binary, {packet, 0}, {active, false}]),
            ok = gen_tcp:send(S, Msg),
            Reply = gen_tcp:recv(S1, Read, 3000),
            {reply, Reply, State = #state{socket = S1}}
    end;

handle_call(list, _From, State = #state{socket = S}) ->
    case timer:now_diff(now(), State#state.last_read) div 1000000 of
        _T when _T > 60  ->
            ok = gen_tcp:send(S, <<1>>),
            {ok, <<Size:32/integer>>} = gen_tcp:recv(S, 4, 3000),
            {ok, Reply} = gen_tcp:recv(S, Size, 3000),
            Ms = decode_metrics(Reply, []),
            {reply, {ok, Ms}, State#state{last_read = now(), metrics=Ms}};
        _ ->
            {reply, {ok, State#state.metrics}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket = S}) ->
    gen_tcp:close(S),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode_metrics(<<>>, Acc) ->
    Acc;
decode_metrics(<<S:16/integer, M:S/binary, R/binary>>, Acc) ->
    decode_metrics(R, [M | Acc]).
