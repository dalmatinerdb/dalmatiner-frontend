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
-export([start_link/2, build/1, emit/3, done/1, start/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type parent() :: {reference(), pid()}.
-type query_step() :: {Module::atom(), Args::list()}.
-type child() :: {reference(), query_step()}.


-type child_queries() ::
        [child()] |
        [query_step()] |
        child() |
        query_step().

-callback init(Query :: query_step()) ->
    {ok, State::term(), ChildQueries::child_queries()}.


-callback start(Start::integer(), Count::integer(), Parents::[parent()],
                State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), Resolution::integer(), State::term()} |
    {done, Data::term(), Resolution::integer(), State::term()} |
    {done, State::term()}.


-callback emit(Child::reference(), Data::term(), Resolution::integer(),
               Parents::[parent()], State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), Resolution::integer(), State::term()} |
    {done, Data::term(), Resolution::integer(), State::term()} |
    {done, State::term()}.

-callback done(Child::reference()| {last, reference()} , Parents::[{parent()}],
               State::term()) ->
    {ok, State::term()} |
    {emit, Data::term(), Resolution::integer(), State::term()} |
    {done, Data::term(), Resolution::integer(), State::term()} |
    {done, State::term()}.

-record(state, {
          callback_module,
          callback_state,
          parents,
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
start_link(Parent, Query) ->
    gen_server:start_link(?MODULE, [Parent, Query], []).

emit(Parents, Data, Resolution) ->
    [gen_server:cast(Parent, {emit, Ref, Data, Resolution}) ||
        {Ref, Parent} <- Parents].

done(Parents) ->
    [gen_server:cast(Parent, {done, Ref}) ||
        {Ref, Parent} <- Parents].

start(Pid, Start, Count) ->
    gen_server:cast(Pid, {start, Start, Count}).

build(Query) ->
    Ref = make_ref(),
    {ok, Pid} = supervisor:start_child(dflow_sup, [{Ref, self()}, Query]),
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
init([Parent, {Module, Args}]) ->
    {ok, CState, SubQs} = Module:init(Args),
    Children = [begin
                    {ok, Pid} = start_link({Ref, self()}, Query),
                    {Ref, Pid}
                end || {Ref, Query} <- ensure_refed(SubQs, [])],
    {ok, #state{
            callback_module = Module,
            callback_state = CState,
            parents = [Parent],
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
handle_cast({start, Start, Count},
            State = #state{callback_state = CState,
                           children = Children,
                           callback_module = Mod,
                           parents = Parents}) ->
    CallbackReply = Mod:start(Start, Count,  Parents, CState),
    [start(Pid, Start, Count) || {_, Pid} <- Children],
    handle_callback_reply(CallbackReply, State);

handle_cast({emit, Ref, Data, Resolution},
            State = #state{callback_state = CState,
                           callback_module = Mod,
                           parents = Parents}) ->
    CallbackReply = Mod:emit(Ref, Data, Resolution,  Parents, CState),
    handle_callback_reply(CallbackReply, State);

handle_cast({done, Ref}, State = #state{children = Children,
                                        callback_state = CState,
                                        callback_module = Mod,
                                        parents = Parents}) ->
    {State1, CRef} = case Children of
                         [{Ref, _}] ->
                             {State#state{children = []}, {last, Ref}};
                         Children ->
                             Children1 = lists:keydelete(Ref, 1, Children),
                             {State#state{children = Children1}, Ref}
                     end,
    CallbackReply = Mod:done(CRef, Parents, CState),
    handle_callback_reply(CallbackReply, State1);

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
