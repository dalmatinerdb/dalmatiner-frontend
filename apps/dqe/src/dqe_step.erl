%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@Schroedinger.fritz.box>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 11 Dec 2014 by Heinz Nikolaus Gies <heinz@Schroedinger.fritz.box>
%%%-------------------------------------------------------------------
-module(dqe_step).

-behaviour(gen_server).

%% API
-export([start_link/5, prepare/1, start/1, apply_times/2]).

-ignore_xref([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(GET_CHUNK, 100000). %% needs to be smaller then max_count in the
%% connection getter
-record(state, {parents, children, function, acc, resolution}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Parent, {named, _, Query}, Start, Count, Res) ->
    gen_server:start_link(?MODULE, [Parent, Query, Start, Count, Res], []);

start_link(Parent, Query, Start, Count, Res) ->
    gen_server:start_link(?MODULE, [Parent, Query, Start, Count, Res], []).

prepare({Querys, Start, Count, Resolution, _, _}) ->
     [begin
          {ok, Pid} = supervisor:start_child(
                        dqe_step_sup,
                        [self(), {print, Query}, Start, Count, Resolution]),
          Pid
      end || Query <- Querys].

prepare(Querys, Start, Count, Resolution) when is_list(Querys)->
    [begin
         {ok, Pid} = start_link(self(), Query, Start, Count, Resolution),
         Pid
     end || Query <- Querys];

prepare(Query, Start, Count, Resolution) ->
    prepare([Query], Start, Count, Resolution).


start(Pid) ->
    gen_server:cast(Pid, start).

emit(Pid, Data, Resolution) ->
    gen_server:cast(Pid, {emit, self(), Data, Resolution}).

done(Pid) ->
    gen_server:cast(Pid, {done, self()}).

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

init([Parent, {print, SubQ}, Start, Count, Resolution]) ->
    Children = prepare(SubQ, Start, Count, Resolution),
    {ok, #state{parents = [Parent], children = Children, acc = <<>>,
                function = print}};

init([Parent, {aggr, Aggr, SubQ}, Start, Count, Resolution]) ->
    Children = prepare(SubQ, Start, Count, Resolution),
    {ok, #state{parents = [Parent], children = Children, acc = <<>>,
                function = {aggr, Aggr}}};

init([Parent, {aggr, Aggr, SubQ, Time}, Start, Count, Resolution]) ->
    Children = prepare(SubQ, Start, Count, Resolution),
    {ok, #state{parents = [Parent], children = Children, acc = <<>>,
                function = {aggr, Aggr, Time}}};

init([Parent, {aggr, Aggr, SubQ, Time, Arg}, Start, Count, Resolution]) ->
    Children = prepare(SubQ, Start, Count, Resolution),
    {ok, #state{parents = [Parent], children = Children, acc = <<>>,
                function = {aggr, Aggr, Time, Arg}}};

init([Parent, {get, {Bucket, Metric}}, Start, Count, Resolution]) ->
    {ok, #state{parents = [Parent], function = {get, Bucket, Metric, Start,
                                                Count, Resolution}}}.

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
handle_cast(start, State = #state{function = {get, Bucket, Metric, Start,
                                              Count, _Resolution}}) ->
    do_get(Bucket, Metric, Start, Count, State),
    done_all(State),
    {stop, normal, State};

handle_cast(start, State = #state{function = print, children = Children}) ->
    [start(Child) || Child <- Children],
    {noreply, State#state{acc = now()}};

handle_cast(start, State = #state{children = Children}) ->
    [start(Child) || Child <- Children],
    {noreply, State};

handle_cast({done, C}, State = #state{function = print, children = [C],
                                      acc = T0}) ->
    D = timer:now_diff(now(), T0),
    io:format("> done in ~pms.~n", [round(D/1000)]),
    done_all(State),
    {stop, normal, State};

handle_cast({done, C}, State = #state{acc = <<>>, children = [C]}) ->
    done_all(State),
    {stop, normal, State};

%% Aggregate without a time don't need to keep a accumulator so there is
%% nothing to do when the children are done.
handle_cast({done, C}, State = #state{children = [C], function = {aggr, _Aggr}}) ->
    done_all(State),
    {stop, normal, State};

%% Multiply and devide do not aggregate so there is no accumulator to flush
handle_cast({done, C}, State = #state{children = [C], function = {aggr, F, _}})
  when F =:= multiply;
       F =:= divide ->
    done_all(State),
    {stop, normal, State};

handle_cast({done, C}, State = #state{acc = Acc, children = [C],
                                      resolution = Resolution,
                                      function = {aggr, Aggr, Time}}) ->
    Time1 = apply_times(Time, Resolution),
    Result = mmath_aggr:Aggr(Acc, Time1),
    emit_all(Result, Time1 * Resolution, State),
    done_all(State),
    {stop, normal, State};

handle_cast({done, Child}, State = #state{children = Children}) ->
    {noreply, State#state{children = lists:delete(Child, Children)}};

handle_cast({emit, _From, Data, _Resolution},
            State = #state{function = print}) ->
    io:format("> ~p.~n", [mmath_bin:to_list(Data)]),
    {noreply, State};

%% Aggregate without a time don't need to keep a accumulator so we
%% can simply pass through the data.
handle_cast({emit, _From, Data, Resolution},
            State = #state{function = {aggr, Aggr}}) ->
    Result = mmath_aggr:Aggr(Data),
    emit_all(Result, Resolution, State),
    {noreply, State};

%% Multiply and Divide are special cases since they take 1 argument
%% but do not aggregate, we have to handle them seperately.
handle_cast({emit, _From, Data, Resolution},
            State = #state{function = {aggr, multiply, V}}) ->
    Result = mmath_aggr:scale(Data, V),
    emit_all(Result, Resolution, State),
    {noreply, State};

handle_cast({emit, _From, Data, Resolution},
            State = #state{function = {aggr, divide, V}}) ->
    Result = mmath_aggr:scale(Data, 1/V),
    emit_all(Result, Resolution, State),
    {noreply, State};

handle_cast({emit, _From, Data, Resolution},
            State = #state{function = {aggr, Aggr, Time}, acc = Acc}) ->
    Acc1 = <<Acc/binary, Data/binary>>,
    Time1 = apply_times(Time, Resolution),
    %%io:format("~p = apply_times(~p, ~p)~n",[Time1, Time, Resolution]),
    %%io:format("~s(<<...~p...>>, ~p)~n",[Aggr, byte_size(Acc1), Time1]),
    Acc2 = execute(Aggr, Acc1, Time1, Time1 * Resolution, <<>>, State),
    {noreply, State#state{acc = Acc2, resolution = Resolution}};

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
terminate(_Reason, _State) ->
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

emit_all(Data, Resolution, #state{parents = Parents}) ->
    [emit(Parent, Data, Resolution) || Parent <- Parents].

done_all(#state{parents = Parents}) ->
    [done(Parent) || Parent <- Parents].

apply_times(T, R) ->
    to_ms(T) div R.

to_ms({time, N, ms}) ->
    N;
to_ms({time, N, s}) ->
    N*1000;
to_ms({time, N, m}) ->
    N*1000*60;
to_ms({time, N, h}) ->
    N*1000*60*60;
to_ms({time, N, d}) ->
    N*1000*60*60*24;
to_ms({time, N, w}) ->
    N*1000*60*60*24*7.


execute({Aggr, Arg}, Acc, T1, Res1, AccEmit, State) when byte_size(Acc) >= T1 * 9 ->
    MinSize = T1 * 9,
    <<Data:MinSize/binary, Acc1/binary>> = Acc,
    Result = mmath_aggr:Aggr(Data, T1, Arg),
    execute({Aggr, Arg}, Acc1, T1, Res1, <<AccEmit/binary, Result/binary>>, State);

execute(Aggr, Acc, T1, Res1, AccEmit, State) when byte_size(Acc) >= T1 * 9 ->
    MinSize = T1 * 9,
    <<Data:MinSize/binary, Acc1/binary>> = Acc,
    Result = mmath_aggr:Aggr(Data, T1),
    execute(Aggr, Acc1, T1, Res1, <<AccEmit/binary, Result/binary>>, State);

execute(_, Acc, _, _, <<>>, _) ->
    Acc;

execute(_, Acc, _, Res1, AccEmit, State) ->
    emit_all(AccEmit, Res1, State),
    Acc.

do_get(_, _, _, 0, _) ->
    ok;

do_get(Bucket, Metric, Start, Count, State) when Count >= ?GET_CHUNK ->
    {ok, Res, Data} = dalmatiner_connection:get(Bucket, Metric, Start, ?GET_CHUNK),
    emit_all(Data, Res, State),
    do_get(Bucket, Metric, Start + ?GET_CHUNK, Count - ?GET_CHUNK, State);

do_get(Bucket, Metric, Start, Count, State) -> 
    {ok, Res, Data} = dalmatiner_connection:get(Bucket, Metric, Start, Count),
    emit_all(Data, Res, State),
    ok.
