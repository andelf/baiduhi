%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(hi_state).

-behaviour(gen_server).

%% API
-export([start_link/0, seq/0, seq/1, uid/0, uid/1, get/1, set/2, keys/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {seq, uid}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

uid() ->
    gen_server:call(?SERVER, get_uid).

uid(Uid) ->
    gen_server:call(?SERVER, {set_uid, Uid}).

seq() ->
    gen_server:call(?SERVER, {get_seq_then_add, 1}).

seq(no_increase) ->
    gen_server:call(?SERVER, {get_seq_then_add, 0});

seq(Seq) ->
    gen_server:cast(?SERVER, {set_seq, Seq}).

get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

set(Key, Val) ->
    gen_server:call(?SERVER, {set, Key, Val}).

keys() ->
    gen_server:call(?SERVER, get_keys).


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
init([]) ->
    {ok, #state{seq=0, uid=0}}.

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
handle_call({set_uid, Uid}, _From, State) ->
    Reply = {ok, Uid},
    {reply, Reply, State#state{uid=Uid}};

handle_call(get_uid, _From, #state{uid=Uid}=State) ->
    Reply = Uid,
    {reply, Reply, State};

handle_call({get_seq_then_add, Adder}, _From, #state{seq=Seq}=State) ->
    Reply = Seq,
    {reply, Reply, State#state{seq=Seq+Adder}};
handle_call({get, Key}, _From, State) ->
    Reply = erlang:get(Key),
    {reply, Reply, State};
handle_call({set, Key, Val}, _From, State) ->
    Reply = erlang:put(Key, Val),
    {reply, Reply, State};
handle_call(get_keys, _From, State) ->
    Reply = erlang:get(),
    {reply, Reply, State};
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
handle_cast({set_seq, Seq}, State) ->
    {noreply, State#state{seq=Seq}};
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
