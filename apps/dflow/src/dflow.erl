%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@Schroedinger.fritz.box>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 12 Dec 2014 by Heinz Nikolaus Gies <heinz@Schroedinger.fritz.box>
%%%-------------------------------------------------------------------
-module(dflow).

-behaviour(gen_server).

%% API
-export([start_link/3, build/1, emit/3, done/1, start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_Q_LEN, 20).


-type query_step() :: {Module::atom(), Args::list()}.
-type child() :: {reference(), query_step()}.


-type child_queries() ::
        [child()] |
        [query_step()] |
        child() |
        query_step().

-callback init(Query :: query_step()) ->
    {ok, State::term(), ChildQueries::child_queries()}.


-callback start(Payload::term(), State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), Resolution::integer(), State::term()} |
    {done, Data::term(), Resolution::integer(), State::term()} |
    {done, State::term()}.


-callback emit(Child::reference(), Data::term(), Resolution::integer(),
               State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), Resolution::integer(), State::term()} |
    {done, Data::term(), Resolution::integer(), State::term()} |
    {done, State::term()}.

-callback done(Child::reference()| {last, reference()}, State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), Resolution::integer(), State::term()} |
    {done, Data::term(), Resolution::integer(), State::term()} |
    {done, State::term()}.

-record(state, {
          callback_module,
          callback_state,
          parents,
          start_count = 0,
          parent_count = 1,
          children
         }).

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
start_link(Parent, Query, Queries) ->
    gen_server:start_link(?MODULE, [Parent, Query, Queries], []).

emit(Parents, Data, Resolution) ->
    [emit(Parent, Ref, Data, Resolution) || {Ref, Parent} <- Parents].

emit(Pid, Ref, Data, Resolution) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, N} when N > ?MAX_Q_LEN ->
            gen_server:call(Pid, {emit, Ref, Data, Resolution}, infinity);
        _ ->
            gen_server:cast(Pid, {emit, Ref, Data, Resolution})
    end.

done(Parents) ->
    [gen_server:cast(Parent, {done, Ref}) ||
        {Ref, Parent} <- Parents].

start(Pid, Payload) ->
    gen_server:cast(Pid, {start, Payload}).

add_parent(Pid, Ref) ->
    link(Pid),
    gen_server:call(Pid, {add_parent, {Ref, self()}}).

build(Query) ->
    Ref = make_ref(),
    {ok, Pid} = supervisor:start_child(
                  dflow_sup, [{Ref, self()}, Query, dict:new()]),
    receive
        {queries, Ref, _} ->
            ok
    after
        1000 ->
            error(timeout)
    end,
    {ok, Ref, Pid}.

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
init([{PRef, Parent}, {Module, Args}, Queries]) ->
    {ok, CState, SubQs} = Module:init(Args),
    {Queries1, Children} =
        lists:foldl(
          fun ({Ref, Query}, {QAcc, CAcc}) ->
                  case dict:find(Query, QAcc) of
                      error ->
                          {ok, Pid} = start_link({Ref, self()}, Query, QAcc),
                          receive
                              {queries, Ref, QAcc1} ->
                                  QAcc2 = dict:store(Query, Pid, QAcc1),
                                  {QAcc2, [{Ref, Pid} | CAcc]}
                          after
                              1000 ->
                                  error(timeout)
                          end;
                      {ok, Pid} ->
                          add_parent(Pid, Ref),
                          {QAcc, [{Ref, Pid} | CAcc]}
                  end
          end, {Queries, []}, ensure_refed(SubQs, [])),
    Parent ! {queries, PRef, Queries1},
    {ok, #state{
            callback_module = Module,
            callback_state = CState,
            parents = [{PRef, Parent}],
            children = Children
           }}.

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
handle_call({add_parent, Parent}, _From,
            State = #state{parents = Parents, parent_count = Count}) ->
    {reply, ok, State#state{parents = [Parent | Parents],
                            parent_count = Count + 1}};

handle_call({emit, Ref, Data, Resolution}, _From,
            State = #state{callback_state = CState,
                           callback_module = Mod}) ->
    CallbackReply = Mod:emit(Ref, Data, Resolution, CState),
    case handle_callback_reply(CallbackReply, State) of
        {noreply, State1} ->
            {reply, ok, State1};
         Stop ->
            Stop
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
handle_cast({start, Payload},
            State = #state{callback_state = CState,
                           children = Children,
                           callback_module = Mod,
                           start_count = SCount,
                           parent_count = PCount}) when PCount =:= SCount + 1 ->
    CallbackReply = Mod:start(Payload, CState),
    [start(Pid, Payload) || {_, Pid} <- Children],
    handle_callback_reply(CallbackReply, State);

handle_cast({start, _Payload}, State = #state{start_count = Count}) ->
    {noreply, State#state{start_count = Count + 1}};


handle_cast({emit, Ref, Data, Resolution},
            State = #state{callback_state = CState,
                           callback_module = Mod}) ->
    CallbackReply = Mod:emit(Ref, Data, Resolution, CState),
    handle_callback_reply(CallbackReply, State);

handle_cast({done, Ref}, State = #state{children = Children,
                                        callback_state = CState,
                                        callback_module = Mod}) ->
    {State1, CRef} = case Children of
                         [{Ref, _}] ->
                             {State#state{children = []}, {last, Ref}};
                         Children ->
                             Children1 = lists:keydelete(Ref, 1, Children),
                             {State#state{children = Children1}, Ref}
                     end,
    CallbackReply = Mod:done(CRef, CState),
    handle_callback_reply(CallbackReply, State1).

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



ensure_refed([], Acc) ->
    Acc;
ensure_refed([{Ref, Q} | T], Acc) when is_reference(Ref) ->
    ensure_refed(T, [{Ref, Q} | Acc]);
ensure_refed([Q | T], Acc) ->
    ensure_refed(T, [{make_ref(), Q} | Acc]);
ensure_refed(Q, []) ->
    [{make_ref(), Q}].

handle_callback_reply({ok, CState1}, State) ->
    {noreply, State#state{callback_state = CState1}};

handle_callback_reply({emit, Data, Resolution, CState1},
                      State = #state{parents = Parents}) ->
    emit(Parents, Data, Resolution),
    {noreply, State#state{callback_state = CState1}};
handle_callback_reply({done, Data, Resolution, CState1},
                      State = #state{parents = Parents}) ->
    emit(Parents, Data, Resolution),
    done(Parents),
    {stop, normal, State#state{callback_state = CState1}};

handle_callback_reply({done, CState1}, State = #state{parents = Parents}) ->
    done(Parents),
    {stop, normal, State#state{callback_state = CState1}}.
