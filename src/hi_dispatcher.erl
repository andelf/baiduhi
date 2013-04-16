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
                                     gathered=Gathered,
                                     client_process=ClientPid,
                                     aeskey=AESKey,
                                     timeout=Timeout} = State) ->
    RawData = <<Gathered/binary, Bin/binary>>,
    if
        byte_size(RawData) < 4 * 10 ->
            {noreply, State#state{gathered=RawData}};
        true ->
            <<?BIN_PRO_VER_1_0:?UINT32,
              ?CT_TAG:?UINT32,
              0:2, HeartBeat:1, Compress:1, Encrypt:1, ConFlag:3, 0:24,
              SrcDataLen:?UINT32,
              ZipDataLen:?UINT32,
              DestDataLen:?UINT32,
              _SendFlag:?UINT32,
              _Category:?UINT32,
              0:64,                            % padding
              Rest/binary>> = RawData,
            if  byte_size(Rest) < DestDataLen ->
                    {noreply, State#state{gathered=RawData}};
                ConFlag =:= ?CT_FLAG_CON_S2 ->
                    <<DestData:DestDataLen/binary, Rest1/binary>> = Rest,
                    <<ConMethod:?UINT8, RootKeyNo:?UINT8, RootKeyLen:?UINT32,
                      0:?UINT32, 0:?UINT32,
                      DataLen:?UINT32,
                      Data:DataLen/binary>> = DestData,
                    ClientPid ! {stage2, {ConMethod, RootKeyNo, RootKeyLen, Data}},
                    {noreply, State#state{gathered=Rest1}};
                ConFlag =:= ?CT_FLAG_CON_S4 ->
                    <<DestData:DestDataLen/binary, Rest1/binary>> = Rest,
                    <<Seed:?RANDOM_KEY_SEED_LEN/binary, KeepAliveSpace:?UINT32,
                      0:?UINT32, 0:?UINT32,
                      DataLen:?UINT32,
                      Data:DataLen/binary,
                      "<ts_config><heartbeat sign_interval=\"40\" ",
                      "echo_timeout=\"80\"/></ts_config>">> = DestData,
                    ClientPid ! {stage4, {Seed, KeepAliveSpace, Data}},
                    {noreply, State#state{gathered=Rest1}};
                HeartBeat =:= 1 ->
                    <<_DestData:DestDataLen/binary, Rest1/binary>> = Rest,
                    hi_heartbeat ! heartbeat,
                    %% heartbeat 1.0 N 0, method:heartbeat, uid:65858906
                    {noreply, State#state{gathered=Rest1}, Timeout};
                Encrypt =:= 1 ->
                    <<DestData:DestDataLen/binary, Rest1/binary>> = Rest,
                    <<ZipData:ZipDataLen/binary, _/binary>> = util:aes_decrypt(DestData, AESKey),
                    if
                        Compress =:= 1 ->
                            <<SrcData:SrcDataLen/binary, _/binary>> = zlib:uncompress(ZipData);
                        true ->
                            <<SrcData:SrcDataLen/binary, _/binary>> = ZipData
                    end,
                    case protocol:decode_impacket(SrcData) of
                        {{_, _, ack, Seq}, _, _} = IMPacket ->
                            case hi_state:get(Seq) of
                                undefined ->
                                    ClientPid ! {impacket, IMPacket},
                                    {noreply, State#state{gathered=Rest1}, Timeout};
                                {Pid, Ref} ->
                                    Pid ! {impacket_ack, Ref, IMPacket},
                                    {noreply, State#state{gathered=Rest1}, Timeout}
                            end;
                        {_, _, _} = IMPacket ->
                            ClientPid ! {impacket, IMPacket},
                            {noreply, State#state{gathered=Rest1}, Timeout};
                        _Other ->
                            {stop, "packet decode error!", State}
                    end
            end
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
