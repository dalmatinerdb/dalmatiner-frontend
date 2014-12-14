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
-export([start_link/3, build/1, emit/3, done/1, start/2,
         describe/1, desc_to_graphvix/1,
         terminate/1]).

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

-callback describe(State::term()) ->
    Description::string() | binary().

-callback start(Payload::term(), State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), State::term()} |
    {done, Data::term(), State::term()} |
    {done, State::term()}.


-callback emit(Child::reference(), Data::term(), State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), State::term()} |
    {done, Data::term(), State::term()} |
    {done, State::term()}.

-callback done(Child::reference()| {last, reference()}, State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), State::term()} |
    {done, Data::term(), State::term()} |
    {done, State::term()}.

-record(state, {
          callback_module,
          callback_state,
          parents,
          start_count = 0,
          parent_count = 1,
          children,
          completed_children = [],
          in = 0,
          out = 0,
          running = false
         }).

%%%===================================================================
%%% API
%%%===================================================================

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

describe(Pid) ->
    graph(Pid).

desc_to_graphvix(Desc) ->
    Edges = lists:usort(flatten(Desc, [])),
    ["digraph {\n", [to_gviz(Edge) || Edge <- Edges], "}"].


start(Pid, Payload) ->
    gen_server:cast(Pid, {start, Payload}).

terminate(Pid) ->
    supervisor:terminate_child(dflow_sup, Pid).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Parent, Query, Queries) ->
    gen_server:start_link(?MODULE, [Parent, Query, Queries], []).



emit(Parents, Data) ->
    [emit(Parent, Ref, Data) || {Ref, Parent} <- Parents].

emit(Pid, Ref, Data) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, N} when N > ?MAX_Q_LEN ->
            gen_server:call(Pid, {emit, Ref, Data}, infinity);
        _ ->
            gen_server:cast(Pid, {emit, Ref, Data})
    end.

to_gviz({label, From, Label}) ->
    [pid_to_list(From), " [label=\"", Label, "\"];\n"];


%% We swap to and frop because we want arrows pointing from the lower to the
%% higher.
to_gviz({edge, From, To}) ->
    [pid_to_list(To), " -> ", pid_to_list(From), ";\n"].

flatten({Pid, Desc, Children}, Acc) ->
    Acc1 = [{label, Pid, Desc} | Acc],
    lists:foldl(fun ({CPid, CMod, CChildren}, FAcc) ->
                        FAcc1 = [{edge, Pid, CPid} | FAcc],
                        flatten({CPid, CMod, CChildren}, FAcc1)
                end, Acc1, Children).

graph(Pid) ->
    gen_server:call(Pid, graph, infinity).

done(Parents) ->
    [gen_server:cast(Parent, {done, Ref}) ||
        {Ref, Parent} <- Parents].

add_parent(Pid, Ref) ->
    link(Pid),
    gen_server:call(Pid, {add_parent, {Ref, self()}}).

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

handle_call({emit, Ref, Data}, _From,
            State = #state{callback_state = CState,
                           callback_module = Mod}) ->
    CallbackReply = Mod:emit(Ref, Data, CState),
    State1 = handle_callback_reply(CallbackReply, State),
    {reply, ok, State1};

handle_call(graph, _, State = #state{children = Children,
                                     completed_children = Completed,

                                     callback_state = CState,
                                     callback_module = Mod}) ->

    Children1 = [graph(Child) || {_, Child} <- Children ++ Completed],
    Desc = [format_in(State), Mod:describe(CState), format_out(State)],
    {reply, {self(), Desc, Children1}, State};

handle_call(terminate, _From, State = #state{}) ->
    {stop, normal, State};

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
    State1 = handle_callback_reply(CallbackReply, State),
    {noreply, State1};

handle_cast({start, _Payload}, State = #state{start_count = Count}) ->
    {noreply, State#state{start_count = Count + 1}};


handle_cast({emit, Ref, Data},
            State = #state{callback_state = CState,
                           callback_module = Mod, in = In}) ->
    CallbackReply = Mod:emit(Ref, Data, CState),
    State1 = handle_callback_reply(CallbackReply, State#state{in = In+1}),
    {noreply, State1};

handle_cast({done, Ref}, State = #state{children = Children,
                                        completed_children = Completed,
                                        callback_state = CState,
                                        callback_module = Mod}) ->
    {State1, CRef} = case Children of
                         [{Ref, _} = C] ->
                             {State#state{children = [],
                                          completed_children = [C|Completed]},
                              {last, Ref}};
                         Children ->
                             C = lists:keyfind(Ref, 1, Children),
                             Children1 = lists:keydelete(Ref, 1, Children),
                             {State#state{children = Children1,
                                          completed_children = [C|Completed]},
                              Ref}
                     end,
    CallbackReply = Mod:done(CRef, CState),
    State2 = handle_callback_reply(CallbackReply, State1),
    {noreply, State2}.

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
    State#state{callback_state = CState1};

handle_callback_reply({emit, Data, CState1},
                      State = #state{parents = Parents, out = Out}) ->
    emit(Parents, Data),
    State#state{callback_state = CState1, out = Out + 1};

handle_callback_reply({done, Data, CState1},
                      State = #state{parents = Parents, out = Out}) ->
    emit(Parents, Data),
    done(Parents),
    State#state{callback_state = CState1, out = Out + 1};

handle_callback_reply({done, CState1}, State = #state{parents = Parents}) ->
    done(Parents),
    State#state{callback_state = CState1}.


format_in(#state{in = 0}) ->
    "";
format_in(#state{in = V}) ->
    ["[", integer_to_list(V), "]\\nV\\n"].


format_out(#state{out = 0}) ->
    "";
format_out(#state{out = V}) ->
    ["\\n[", integer_to_list(V), "]\\nV"].
