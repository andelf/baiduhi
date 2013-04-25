%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(hi_dispatcher).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, set_aeskey/1,
         set_sock/1, set_client/1, set_client/0, get_pid/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% 200ms
-define(TIMEOUT, 0).

-define(SERVER, ?MODULE).

-include("const.hrl").

-record(state, {sock,
                gathered,
                client_process,
                timeout,
                aeskey}).

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

stop() ->
    gen_server:cast(?SERVER, stop).

set_sock(Sock) ->
    gen_server:call(?SERVER, {set_sock, Sock}).

set_aeskey(AESKey) ->
    gen_server:call(?SERVER, {set_aeskey, AESKey}).

set_client() ->
    gen_server:call(?SERVER, {set_client, self()}).

set_client(ClientPid) ->
    gen_server:call(?SERVER, {set_client, ClientPid}).

get_pid() ->
    gen_server:call(?SERVER, {get_pid}).

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
    {ok, #state{timeout=?TIMEOUT}}.

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
handle_call({set_sock, Sock}, _From, State) ->
    Reply = inet:setopts(Sock, [{active, true}]),
    {reply, Reply, State#state{sock=Sock, gathered= <<>>}};
handle_call({set_client, Pid}, {Pid, _Ref} = _From, State) ->
    Reply = {ok, Pid},
    {reply, Reply, State#state{client_process=Pid}};
handle_call({set_aeskey, AESKey}, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{aeskey=AESKey}};
handle_call({get_pid}, _From, State) ->
    Reply = self(),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    io:format("hi_dispatcher req ~p from ~p state ~p", [_Request, _From, State]),
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
handle_cast(stop, State) ->
    {stop, normal, State};
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
handle_info(timeout, #state{sock=Sock} = State) ->
    self() ! {tcp, Sock, <<>>},
    {noreply, State};

handle_info({tcp, Sock, Bin}, #state{sock=Sock,
                                     gathered=Gathered} = State) ->
    RawData = <<Gathered/binary, Bin/binary>>,
    case protocol:raw_packet_precheck(RawData) of
        partical ->
            {noreply, State#state{gathered=RawData}};
        ok ->
            handle_packet(RawData, State)
    end;
handle_info({tcp_closed, _What}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    io:format("hi_dispatcher got unkown message ~w~n", [_Info]),
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
handle_packet(RawData, #state{client_process=ClientPid,
                              timeout=Timeout} = State) ->
    {Packet, Rest} = protocol:decode_packet(RawData),
    %% error_logger:info_msg("got packet ~p~n", [Packet]),
    case Packet of
        {stage2, _} ->
            ClientPid ! Packet;
        {stage4, _} ->
            ClientPid ! Packet;
        {heartbeat} ->
            hi_heartbeat ! heartbeat;
        {impacket, {{_,_,ack,Seq},_,_} = IMPacket} ->
            case hi_state:get(Seq) of
                undefined ->
                    ClientPid ! {impacket, IMPacket};
                {Pid, Ref} ->
                    Pid ! {impacket_ack, Ref, IMPacket}
            end;
        {impacket, {_, _, _} = IMPacket} ->
            ClientPid ! {impacket, IMPacket};
        {error, _} ->
            io:format("impacket decode error!~n")
    end,
    {noreply, State#state{gathered=Rest}, Timeout}.
