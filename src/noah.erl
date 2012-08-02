%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 29 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(noah).

-behaviour(gen_server).

%% API
-export([start_link/0,
         lookup_monitor_item/2,
         lookup_node/1,
         exit/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 400000).
-record(state, {port}).

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

lookup_monitor_item(NodeId, MonitorItemName) ->
    gen_server:call(?SERVER, {lookup_monitor_item, NodeId, MonitorItemName}).

lookup_node(Path) ->
    gen_server:call(?SERVER, {lookup_node, Path}).

exit() ->
    gen_server:cast(?SERVER, {exit, normal}).
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
    {ok, #state{}, 0}.

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
%handle_call({lookup_monitor_item, NodeId, MonitorItemName}, _From, #state{port=Port} = State) ->
handle_call(CommandTuple, _From, #state{port=Port} = State) ->
    ReqData = term_to_binary(CommandTuple),
    port_command(Port, ReqData),
    io:format("calling~n"),
    Reply = receive
                {Port, {data, RespData}} ->
                    case binary_to_term(RespData) of
                        'none' ->
                            {error, no_result};
                        Ret ->
                            {ok, Ret}
                    end
            after
                ?TIMEOUT ->
                    {error, timeout}
            end,
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
handle_cast(CommandTuple, #state{port=Port} = State) ->
    ReqData = term_to_binary(CommandTuple),
    port_command(Port, ReqData),
    {stop, "cast will cause stop", State}.

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
handle_info(timeout, State) ->
    NoahPortPy = filename:join(code:priv_dir(baiduhi), "noah_port.py"),
    Port = open_port({spawn, "python -u " ++ NoahPortPy},
                     [{packet, 1}, binary, use_stdio]),
    {noreply, State#state{port=Port}};
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
