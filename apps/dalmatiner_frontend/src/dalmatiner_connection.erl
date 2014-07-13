%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 23 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dalmatiner_connection).

-include_lib("dproto/include/dproto.hrl").

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1, get/4, list/1, list/0]).
-ignore_xref([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 30000).
-define(MAX_COUNT, 604800).
-record(state, {socket, metrics=gb_trees:empty(), host, port}).

%%%===================================================================
%%% API
%%%===================================================================

get(Bucket, Metric, Time, Count) when Count =< ?MAX_COUNT ->
    poolboy:transaction(backend_connection,
                        fun(Worker) ->
                                gen_server:call(Worker, {get, Bucket, Metric, Time, Count}, ?TIMEOUT)
                        end).

list(Bucket) ->
    poolboy:transaction(backend_connection,
                        fun(Worker) ->
                                gen_server:call(Worker, {list, Bucket}, ?TIMEOUT)
                        end).

list() ->
    poolboy:transaction(backend_connection,
                        fun(Worker) ->
                                gen_server:call(Worker, list, ?TIMEOUT)
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
handle_call({get, Bucket, Metric, Time, Count}, _From,
            State = #state{socket = S}) ->
    Msg = <<?GET, (dproto_tcp:encode_get(Bucket, Metric, Time, Count))/binary>>,
    ok = gen_tcp:send(S, Msg),
    Read = Count*9,
    case gen_tcp:recv(S, Read, 3000) of
        {ok, D} ->
            {reply, {ok, D}, State};
        {error, E} ->
            lager:error("[connection/recv] Error: ~p", [E]),
            gen_tcp:close(S),
            {ok, S1} = gen_tcp:connect(State#state.host, State#state.port,
                                       [binary, {packet, 0}, {active, false}]),
            ok = gen_tcp:send(S1, Msg),
            Reply = gen_tcp:recv(S1, Read, 3000),
            {reply, Reply, State#state{socket = S1}}
    end;

handle_call({list, Bucket}, _From, State) ->
    case gb_trees:lookup(Bucket, State#state.metrics) of
        none ->
            {Ms, State1} = do_list(Bucket, State),
            {reply, {ok, Ms}, State1};
        {value, {LastRead, Ms}} ->
            case timer:now_diff(now(), LastRead) div 100000000 of
                T when T > 60  ->
                    io:format("~p - ~p: ~p~n", [now(), LastRead, T/100000000]),
                    {Ms, State1} = do_list(Bucket, State),
                    {reply, {ok, Ms}, State1};
                _ ->
                    {reply, {ok, Ms}, State}
            end
    end;

handle_call(list, _From, State = #state{socket = S}) ->
    Msg = <<?BUCKETS>>,
    ok = gen_tcp:send(S, Msg),
    case gen_tcp:recv(S, 4, 3000) of
        {ok, <<Size:32/integer>>} ->
            {ok, D} = gen_tcp:recv(S, Size, 3000),
            {reply, {ok, decode_metrics(D, [])}, State};
        {error, E} ->
            lager:error("[connection/recv] Error: ~p", [E]),
            gen_tcp:close(S),
            {ok, S1} = gen_tcp:connect(State#state.host, State#state.port,
                                       [binary, {packet, 0}, {active, false}]),
            ok = gen_tcp:send(S1, Msg),
            {ok, <<Size:32/integer>>} = gen_tcp:recv(S, 4, 3000),
            case gen_tcp:recv(S1, Size, 3000) of
                {ok, D} ->
                    {reply, {ok, decode_metrics(D, [])}, State};
                E ->
                    {reply, E, State#state{socket = S1}}
            end
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

do_list(Bucket, State = #state{socket = S}) ->
    ok = gen_tcp:send(S, <<?LIST, (dproto_tcp:encode_list(Bucket))/binary>>),
    case gen_tcp:recv(S, 4, 3000) of
        {ok, <<Size:32/integer>>} ->
            {ok, Reply} = gen_tcp:recv(S, Size, 30000),
            Ms = decode_metrics(Reply, []),
            io:format("metrics: ~p~n", [length(Ms)]),
            {Ms, State#state{metrics = gb_trees:enter(Bucket, {now(), Ms}, State#state.metrics)}};
        {error, _} ->
            gen_tcp:close(S),
            {ok, S1} = gen_tcp:connect(State#state.host, State#state.port,
                                       [binary, {packet, 0}, {active, false}]),
            {ok, <<Size:32/integer>>} = gen_tcp:recv(S, 4, 3000),
            {ok, Reply} = gen_tcp:recv(S, Size, 30000),
            Ms = decode_metrics(Reply, []),
            {Ms, State#state{metrics = gb_trees:enter(Bucket, {now(), Ms}, State#state.metrics),
                             socket = S1}}
    end.
