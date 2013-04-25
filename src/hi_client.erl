%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(hi_client).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/2, stop/0, async_impacket_requet/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("rootkey.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("const.hrl").

-record(state, {sock,
                username,
                stage,
                config=[],
                aeskey=[]
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
start_link() ->
    ConfFile = filename:join(code:priv_dir(baiduhi), "baiduhi.conf"),
    case file:consult(ConfFile) of
        {ok, [{baiduhi, Conf}]} ->
            {username, Username} = lists:keyfind(username, 1, Conf),
            {password, Password} = lists:keyfind(password, 1, Conf),
            start_link(Username, Password);
        Other ->
            {error, {conf_file_error, Other}}
    end.
start_link(Username, Password) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [{username, Username}, {password, Password}], []).

stop() ->
    gen_server:cast(?SERVER, stop).

async_impacket_requet({{_,_,request,Seq},_,_} = ImPacket) ->
    %% evaluate self() in caller process
    Pid = self(),
    Ref = make_ref(),
    hi_state:set(Seq, {Pid, Ref}),
    gen_server:cast(?SERVER, {async_sendpkt, ImPacket}),
    receive
        {impacket_ack, Ref, AckImpacket} ->
            {ok, AckImpacket}
    after 1000 ->
            {error, time_out}
    end.


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
init(Config) ->
    {username, Username} = lists:keyfind(username, 1, Config),
    %% logger:log(normal, "username=~s password=******", [Username]),
    {ok, Sock} = gen_tcp:connect("119.75.214.21", 443, [{active, false}, binary]),
    {ok, #state{sock=Sock, username=Username, config=Config, stage=initial}, 0}.

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
handle_cast({async_sendpkt, {{_,_,request,_},_,_} = IMPacket},
            #state{sock=Sock, aeskey=AESKey} = State) ->
    BinData = protocol:encode_impacket(IMPacket),
    ok = gen_tcp:send(Sock, protocol:make_packet(normal, BinData, AESKey)),
    {noreply, State};
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
%% ------------- login process ----------------------
%% initial login stage1
handle_info(timeout, #state{sock=Sock, config=Config, stage=initial} = State) ->
    {username, Username} = lists:keyfind(username, 1, Config),
    ok = gen_tcp:controlling_process(Sock, hi_dispatcher:get_pid()),
    hi_dispatcher:set_client(self()),
    hi_dispatcher:set_sock(Sock),
    hi_state:set(username, Username),           %set first
    ok = inet:setopts(Sock, [{active, true}]),
    S1Data = protocol:make_stage_data(stage1),
    gen_tcp:send(Sock, protocol:make_packet(stage, ?CT_FLAG_CON_S1, S1Data)),
    {noreply, State#state{username=Username, stage=stage1}};
%% stage 2, stage3
handle_info({stage2, {_ConMethod, RootKeyNo, _RootKeyLen, S2Data}},
            #state{sock=Sock, stage=stage1} = State) ->
    S1Key = lookup_root_pub_key(RootKeyNo),
    S2Key = util:pkcs1_to_rsa_pubkey(util:rsa_public_decrypt(S2Data, S1Key)),
    %% 用S2Key加密S3 pub key
    S3Data = protocol:make_stage_data(
               stage3,
               util:rsa_public_encrypt(binary:list_to_bin(?HiPubKey), S2Key)),
    gen_tcp:send(Sock, protocol:make_packet(stage, ?CT_FLAG_CON_S3, S3Data)),
    {noreply, State#state{stage=stage3}};
%% stage 4, security:verify
handle_info({stage4, {Seed, _KeepAliveSpace, S4Data}},
            #state{config=Config, stage=stage3} = State) ->
    S3PrvKey = util:pkcs1_to_rsa_prvkey(?HiPrvKey),
    AESKey = util:rsa_private_decrypt(S4Data, S3PrvKey),
    hi_dispatcher:set_aeskey(AESKey),
    hi_state:set(aeskey, AESKey),
    error_logger:info_msg("aes key: ~p", [util:to_hex_string(AESKey)]),
    self() ! {sendpkt, protocol_helper:'security#verify'(login)},
    {noreply, State#state{stage=on_security_response,
                          config=[{seed, Seed}|Config],
                          aeskey=AESKey}};
%% login:login
handle_info({impacket, {{security, _, ack, _}, _, Xml}},
            #state{config=Config, stage=on_security_response} = State) ->
    {password, Password} = lists:keyfind(password, 1, Config),
    {seed, Seed} = lists:keyfind(seed, 1, Config),
    [{verify, RequestParams, _}] = util:xml_to_tuple(Xml),
    {v_url, V_Url} = lists:keyfind(v_url, 1, RequestParams),
    {v_time, V_Time} = lists:keyfind(v_time, 1, RequestParams),
    {v_period, V_Period} = lists:keyfind(v_period, 1, RequestParams),
    case lists:keyfind(v_code, 1, RequestParams) of
        {v_code, V_Code} ->
            error_logger:info_msg("v_code: ~s", [V_Code]);
        false ->
            io:format("v_code url: http://vcode.im.baidu.com/cgi-bin/genimg?~s~n", [V_Url]),
            V_Code = io:get_chars("plz input code>", 4)
    end,
    self() ! {sendpkt, protocol_helper:'login#login'({V_Url, V_Time, V_Period, V_Code},
                                                     protocol:build_dynamic_password(Password, Seed))},
    {noreply, State#state{stage=on_login_login_response}};
%% user:login_ready
handle_info({impacket, {{login, _, ack, _}, [{method,"login"},{code,200}|_], Xml}},
            #state{stage=on_login_login_response} = State) ->
    {Doc, _} = xmerl_scan:string(Xml),
    [#xmlAttribute{value=StrUid}] = xmerl_xpath:string("//login/@imid", Doc),
    Uid = list_to_integer(StrUid),              % always use integer uid
    hi_state:uid(Uid),                        % set uid
    hi_state:set(imid, Uid),

    self() ! {sendpkt, protocol_helper:'user#login_ready'()},
    %% empty config state, for safty
    {noreply, State#state{stage=on_login_ready_response, config=[]}};
handle_info({impacket, {{login, _, ack, _}, [{method,"login"},{code,Code}|_], _}},
            #state{stage=on_login_login_response} = State) ->
    io:format("~w~n", [{code, Code}]),
    {stop, normal, State};
%% login ready!
handle_info({impacket, {{user, _, ack, _}, [{method,"login_ready"},{code,200}|_], _}},
            #state{sock=Sock, stage=on_login_ready_response} = State) ->
    hi_heartbeat:set_sock(Sock),
    hi_heartbeat:set_timeout(40000),
    hi_event:user_login_ready(hi_state:uid()),
    {noreply, State#state{stage=normal}};
%% ----------------------------------------
%% Handle request
%% ----------------------------------------
handle_info({impacket, {{msg, _, notify, _}, [{method,"msg_notify"}|Params], Xml}},
            State) ->
    {from, From} = lists:keyfind(from, 1, Params),
    %% type=2 group, type=3 multi
    {type, Type} = lists:keyfind(type, 1, Params),
    %% 联系人ID|群ID|临时群ID
    {to, To} = lists:keyfind(to, 1, Params),
    {msgid, MsgId} = lists:keyfind(msgid, 1, Params),
    %% ack
    case lists:keyfind(waitack, 1, Params) of
        {waitack, _Timeout} ->
            self() ! {sendpkt, protocol_helper:'msg#msg_ack'(Type, From, MsgId)};
        false -> ok
    end,
    ReplyTo = if Type =:= 1 -> From; true -> To end,
    IncomeTextMessage = xmerl_msg:xml_to_list(Xml),
    [IncomeMessage] = util:xml_to_tuple(Xml),
    %% events notify
    hi_event:text_msg_notify(IncomeTextMessage, From, Type, ReplyTo),
    hi_event:msg_notify(IncomeMessage, From, Type, ReplyTo),
    {noreply, State};

%% 临时会话
handle_info({impacket, {{msg,_,request,_}, [{method,"tmsg_request"}|Params], Xml}}, State) ->
    {from, From} = lists:keyfind(from, 1, Params),
    Type = 4,                                   % fake as type 4
    IncomeTextMessage = xmerl_msg:xml_to_list(Xml),
    [IncomeMessage] = util:xml_to_tuple(Xml),
    ReplyTo = From,
    hi_event:text_msg_notify(IncomeTextMessage, From, Type, ReplyTo),
    hi_event:msg_notify(IncomeMessage, From, Type, ReplyTo),
    %% 临时会话不ack
    {noreply, State};

%%--------------------------------------------------------------------
%% message that no need to ack
handle_info({impacket,{{group,_,notify,_}, [{method,"add_member_notify"}|_Params], Xml}}, State) ->
    [{add_member_notify, [{gid, Gid},{manager,Manager}|_], ImidTokens}] = util:xml_to_tuple(Xml),
    Imids = lists:map(fun({member,[{imid,Imid}],_}) -> list_to_integer(Imid) end, ImidTokens),
    hi_event:group_add_member_notify(list_to_integer(Gid), list_to_integer(Manager), Imids),
    {noreply, State};

handle_info({impacket,{{group,_,notify,_}, [{method,"delete_member_notify"}|_Params], Xml}}, State) ->
    [{delete_member_notify, [{gid, Gid},{manager,Manager}|_], ImidTokens}] = util:xml_to_tuple(Xml),
    Imids = lists:map(fun({member,[{imid,Imid}],_}) -> list_to_integer(Imid) end, ImidTokens),
    hi_event:group_delete_member_notify(list_to_integer(Gid), list_to_integer(Manager), Imids),
    {noreply, State};

handle_info({impacket,{{group,_,notify,_}, [{method,"card_change_notify"}|Params], Xml}}, State) ->
    Imid = proplists:get_value(imid, Params),
    Gid = proplists:get_value(gid, Params),
    [{card, Info, []}] = util:xml_to_tuple(Xml),
    hi_event:group_card_change_notify(Gid, Imid, Info),
    {noreply, State};

handle_info({impacket, {{contact, _, notify, _}, [{method, "notify"}|_Params], Xml}}, State) ->
    [{contact, [{imid, Imid}|Params], []}] = util:xml_to_tuple(Xml),
    hi_event:contact_notify(Imid, Params),
    {noreply, State};

handle_info({impacket, {{contact, _, notify, _}, [{method, "active_notify"}|_Params], _Xml}}, State) ->
    %% TODO: handle it
    {noreply, State};

handle_info({impacket, {{friend,_,notify,_},  [{method,"friend_change"}|_Params], _}}, State) ->
    hi_event:friend_change(),
    {noreply, State};

handle_info({impacket, {{msg, _, notify, _}, [{method, "msg_ack_notify"}|_], _}}, State) ->
    {noreply, State};

handle_info({impacket, {{friend, _, notify, _}, [{method,"add_notify"}|_], Xml}}, State) ->
    [{add_notify, Attrs, _}] = util:xml_to_tuple(Xml),
    {imid, From} = lists:keyfind(imid, 1, Attrs),
    {request_note, RequestNote} = lists:keyfind(request_note, 1, Attrs),
    hi_event:friend_add_notify(From, RequestNote),
    {noreply, State};

%% kickout
handle_info({impacket,{{login,_,notify,_},[{method,"kickout"}|_], _}}, State) ->
    {stop, normal, State};

%% handle other request
handle_info({impacket, {{cm, _, request, _}, [{method,"blk"}|Params], _}}, State) ->
    {uid, From} = lists:keyfind(uid, 1, Params),
    hi_event:blink(From),
    {noreply, State};
handle_info({impacket, {{cm, _, request, _}, [{method, "typ"}|Params], _}}, State) ->
    {from, From} = lists:keyfind(from, 1, Params),
    hi_event:typing(From),
    {noreply, State};
handle_info({impacket, {{cm, _, request, _}, [{method, "scene"}|_], _}}, State) ->
    {noreply, State};
%%--------------------------------------------------------------------
%% handle ack
handle_info({impacket, {{msg,_,ack,_}, [{method, _Method}|_], _}}, State) ->
    {noreply, State};

%%--------------------------------------------------------------------
%% sendpkt
handle_info({sendpkt, {{_,_,request,_},_,_} = IMPacket}, #state{sock=Sock, aeskey=AESKey} = State) ->
    Data = protocol:encode_impacket(IMPacket),
    gen_tcp:send(Sock, protocol:make_packet(normal, Data, AESKey)),
    {noreply, State};
handle_info({sendpkt, Data}, #state{sock=Sock, aeskey=AESKey} = State) ->
    gen_tcp:send(Sock, protocol:make_packet(normal, Data, AESKey)),
    {noreply, State};
%%--------------------------------------------------------------------
handle_info(tcp_closed, State) ->
    logger:log(error, "tcp closed!"),
    {stop, tcp_closed, State};
handle_info(_Info, State) ->
    io:format("hi_client: handle_info() unkown term: ~p~n", [_Info]),
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
terminate(_Reason, #state{sock=Sock} = _State) ->
    %% hi_receiver:stop(),
    %% hi_heartbeat:stop(),
    gen_tcp:close(Sock),
    ok;
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
lookup_root_pub_key(KeyNo) ->
    PubPkcs1 = lists:nth(1+KeyNo, ?RootPubKeyPKCS_1),
    util:pkcs1_to_rsa_pubkey(PubPkcs1).

%% lookup_root_prv_key(KeyNo) ->
%%     PrvPkcs1 = lists:nth(1+KeyNo, ?RootPrvKeyPKCS_1),
%%     util:pkcs1_to_rsa_prvkey(PrvPkcs1).
