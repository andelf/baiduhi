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
-export([start_link/0, start_link/2, stop/0, sendpkt_async/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("rootkey.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("const.hrl").

-record(state, {sock,
                username,
                config=[],
                uid=0,
                stage=0,
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
    start_link("image_help", "loveimage").
    %%start_link("video_help", "lovevideo").
    %%start_link("fledna", "lovelili").
start_link(Username, Password) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [{username, Username}, {password, Password}], []).
sendpkt_async(ImPacket) ->
    gen_server:call(?SERVER, {sendpkt_async, ImPacket}).

stop() ->
    gen_server:cast(?SERVER, stop).
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
    logger:log(normal, "username=~s password=******", [Username]),
    {ok, Sock} = gen_tcp:connect('123.125.113.37', 443, [{active, false}, binary]),
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
handle_call({sendpkt_async, {{_,_,request,Seq},_,_} = IMPacket}, From, #state{sock=Sock, aeskey=AESKey} = State) ->
    hi_state:set(Seq, From),                    % register callback path
    Data = protocol:encode_impacket(IMPacket),
    Reply = gen_tcp:send(Sock, protocol:make_packet(normal, Data, AESKey)),
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
    logger:log(debug, "handle_info() got timeout"),
    {username, Username} = lists:keyfind(username, 1, Config),
    %% {ok, Pid} = hi_receiver:start_link(),
    ok = gen_tcp:controlling_process(Sock, hi_receiver:get_pid()),
    hi_receiver:set_client(self()),
    hi_receiver:set_sock(Sock),
    hi_state:set(username, Username),           %set first
    ok = inet:setopts(Sock, [{active, true}]),
    S1Data = protocol:make_stage_data(stage1),
    gen_tcp:send(Sock, protocol:make_packet(stage, ?CT_FLAG_CON_S1, S1Data)),
    {noreply, State#state{username=Username, stage=stage1}};
%% stage 2, stage3
handle_info({stage2, {ConMethod, RootKeyNo, _RootKeyLen, S2Data}},
            #state{sock=Sock, stage=stage1} = State) ->
    logger:log(debug, "S2 ConMethod=~p", [ConMethod]),
    logger:log(debug, "S2 RootKeyNo=~p", [RootKeyNo]),
    S1Key = lookup_root_pub_key(RootKeyNo),
    S2Key = util:pkcs1_to_rsa_pubkey(util:rsa_public_decrypt(S2Data, S1Key)),
    %% 用S2Key加密S3 pub key
    S3Data = protocol:make_stage_data(
               stage3, util:rsa_public_encrypt(binary:list_to_bin(?HiPubKey), S2Key)),
    gen_tcp:send(Sock, protocol:make_packet(stage, ?CT_FLAG_CON_S3, S3Data)),
    {noreply, State#state{stage=stage3}};
%% stage 4, security:verify
handle_info({stage4, {Seed, _KeepAliveSpace, S4Data}},
            #state{config=Config, stage=stage3} = State) ->
    S3PrvKey = util:pkcs1_to_rsa_prvkey(?HiPrvKey),
    AESKey = util:rsa_private_decrypt(S4Data, S3PrvKey),
    hi_receiver:set_aeskey(AESKey),
    logger:log(normal, "aes key: ~w", [AESKey]),
    self() ! {sendpkt, protocol_helper:'security#verify'(login)},
    {noreply, State#state{stage=on_security_response,
                          config=[{seed, Seed}|Config],
                          aeskey=AESKey}};
%% login:login
handle_info({impacket, {{security, _, ack, _}, _, Xml}},
            #state{config=Config,
                   stage=on_security_response} = State) ->
    {password, Password} = lists:keyfind(password, 1, Config),
    {seed, Seed} = lists:keyfind(seed, 1, Config),
    [{verify, RequestParams, _}] = xmerl_impacket:xml_to_tuple(Xml),
    {v_url, V_Url} = lists:keyfind(v_url, 1, RequestParams),
    {v_time, V_Time} = lists:keyfind(v_time, 1, RequestParams),
    {v_period, V_Period} = lists:keyfind(v_period, 1, RequestParams),
    case lists:keyfind(v_code, 1, RequestParams) of
        {v_code, V_Code} ->
            logger:log(normal, "v_code: ~s", [V_Code]);
        false ->
            io:format("v_code url: http://vcode.im.baidu.com/cgi-bin/genimg?~s~n", [V_Url]),
            V_Code = io:get_chars("plz input code>", 4)
    end,
    logger:log(normal, "v_url:~p v_code:~p", [V_Url, V_Code]),
    self() ! {sendpkt, protocol_helper:'login#login'({V_Url, V_Time, V_Period, V_Code}, protocol:build_dynamic_password(Password, Seed))},
    {noreply, State#state{stage=on_login_login_response}};
%% user:login_ready
handle_info({impacket, {{login, _, ack, _}, [{method,"login"},{code,200}|_], Xml}},
            #state{stage=on_login_login_response} = State) ->
    {Doc, _} = xmerl_scan:string(Xml),
    [#xmlAttribute{value=StrUid}] = xmerl_xpath:string("//login/@imid", Doc),
    Uid = list_to_integer(StrUid),              % always use integer uid
    hi_state:uid(Uid),                        % set uid
    hi_state:set(uid, Uid),
    self() ! {sendpkt, protocol_helper:'user#login_ready'()},
    %% empty config state, for safty
    {noreply, State#state{stage=on_login_ready_response,
                          config=[], uid=Uid}};
handle_info({impacket, {{login, _, ack, _}, [{method,"login"},{code,Code}|_], _}},
            #state{stage=on_login_login_response} = State) ->
    io:format("~w~n", [{code, Code}]),
    {stop, normal, State};
%% login ready!
handle_info({impacket, {{user, _, ack, _}, [{method,"login_ready"},{code,200}|_], _}},
            #state{sock=Sock, stage=on_login_ready_response} = State) ->
    hi_heartbeat:set_sock(Sock),
    hi_heartbeat:set_timeout(40000),
    hi_event:login_ready(),
    {noreply, State#state{stage=normal}};
%% ----------------------------------------
%% Handle request
%% ----------------------------------------
%%
handle_info({impacket, {{cm, _, request, _}, [{method,"blk"}|Params], _}},
            State) ->
    {uid, From} = lists:keyfind(uid, 1, Params),
    hi_event:blink(From),
    self() ! {sendpkt, protocol_helper:'cm#blk'(From)},
    {noreply, State};
handle_info({impacket, {{friend, _, notify, _}, [{method,"add_notify"}|_], Xml}},
            State) ->
    [{add_notify, Attrs, _}] = xmerl_impacket:xml_to_tuple(Xml),
    {imid, From} = lists:keyfind(imid, 1, Attrs),
    {request_note, RequestNote} = lists:keyfind(request_note, 1, Attrs),
    hi_event:add_friend(From, RequestNote),
    {noreply, State};

handle_info({impacket, {{msg, _, notify, _}, [{method,"msg_notify"}|Params], Xml}},
            State) ->
    logger:log(normal, "msg:msg_notify ~p ~n~s", [Params, Xml]),
    {from, From} = lists:keyfind(from, 1, Params),
    {type, Type} = lists:keyfind(type, 1, Params), %type=2 group, type=3 multi
    {to, To} = lists:keyfind(to, 1, Params),     %联系人ID|群ID|临时群ID
    {msgid, MsgId} = lists:keyfind(msgid, 1, Params),
    %% ack
    case lists:keyfind(waitack, 1, Params) of
        {waitack, _Timeout} ->
            logger:log(normal, "msg:~w ack from:~w to:~w type:~w",
                       [MsgId, From, To, Type]),
            self() ! {sendpkt, protocol_helper:'msg#msg_ack'(Type, From, MsgId)};
        false -> ok
    end,
    ReplyTo = if Type =:= 1 -> From; true -> To end,
    IncomeMessage = msg_fmt:msg_to_list(Xml),
    hi_event:text_msg(IncomeMessage, From, Type, ReplyTo),
    case IncomeMessage of
        "!qr " ++ Text ->
            case chart_api:make_chart({qr, Text}) of
                {ok, png, Image} ->
                    ReplyBody = make_xml_bin(
                                  {msg, [], [{font, [{n, "Fixedsys"},
                                                     {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#EE9640},
                                                     {cs, 134}],
                                              []},
                                             {text, [{c, "\n"}], []},
                                             msg_fmt:img_tag({imgdata, "png", Image})
                                            ]});
                {error, Error} ->
                    ReplyBody = make_xml_bin(
                                  {msg, [], [{font, [{n, "Fixedsys"},
                                                     {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#0000CC},
                                                     {cs, 134}],
                                              []},
                                             {text, [{c, io_lib:format("error: ~s", [Error])}], []}
                                            ]})
            end,
            SendTo = if Type =:= 1 -> From; true -> To end,
            self() ! {sendpkt, protocol_helper:'msg#msg_request'(Type, SendTo, ReplyBody)},
            {noreply, State};
        "!quit mul" ++ _ ->
            if
                Type =:= 3 ->                   % if multi msg
                    self() ! {sendpkt, protocol_helper:'multi#quit'(To)},
                    {noreply, State};
                true ->
                    {noreply, State}
            end;
        "!debug" ++ _ ->
            self() ! {sendpkt, protocol_helper:'user#set'([{has_camera, "1"}])},
            self() ! {sendpkt, protocol_helper:'contact#query'(["imid", "baiduid", "cli_type", "cli_ver"],
                                                               ["572761548", "406526983"])},
            {noreply, State};
        "!status " ++ What ->
            %% 1 在线, 无消息
            %% 2 忙碌
            %% 3 离开
            %% 4 隐身, 无消息
            %% 5 离线, 但是能收到消息
            %% self() ! {sendpkt, protocol_helper:'user#set'([{status, util:to_list("2;" ++ What)}])},
            self() ! {sendpkt, protocol_helper:'user#set'([{personal_comment, util:to_list(What)}])},
            {noreply, State};
        "!busy " ++ What ->
            self() ! {sendpkt, protocol_helper:'user#set'([{status, util:to_list("2;" ++ What)}])},
            {noreply, State};
        "!away " ++ What ->
            self() ! {sendpkt, protocol_helper:'user#set'([{status, util:to_list("3;" ++ What)}])},
            {noreply, State};
        "!online" ++ _ ->
            self() ! {sendpkt, protocol_helper:'user#set'([{status, util:to_list("1;")}])},
            {noreply, State};
        "!music " ++ What ->
            case string:tokens(What, " - ") of
                [Name, Author] ->
                    self() ! {sendpkt, protocol_helper:'user#set'([{music,
                                                            "\\01" ++ "\\0" ++ util:to_list(Name) ++
                                                                        "\\0" ++ util:to_list(Author) ++ "\\0" ++
                                                                        "未知专辑" ++ "\\0"}])};
                ["off"] ->
                    self() ! {sendpkt, protocol_helper:'user#set'([{music,
                                                            "\\00\\0\\0\\0\\0"}])};
                _Other ->
                    self() ! {sendpkt, protocol_helper:'user#set'([{music,
                                                            "\\01" ++ "\\0" ++ util:to_list(What) ++
                                                                "\\0" ++ "Unkown Author" ++ "\\0" ++
                                                                "未知专辑" ++ "\\0"}])}
            end,
            {noreply, State};
        "!blk" ++ _ ->
            self() ! {sendpkt, protocol_helper:'cm#blk'(From)},
            {noreply, State};
        "!typ" ++ _ ->
            if
                Type =:= 1 ->                   % if not multi msg
                    self() ! {sendpkt, protocol_helper:'cm#typ'(From)};
                true ->
                    ok
            end,
            {noreply, State};
        "!reboot " ++ Text ->
            Reply = "reboot " ++ binary_to_list(unicode:characters_to_binary(Text)) ++ " ...... ok!",
            ReplyBody = make_xml_bin(
                          {msg, [], [{font, [{n, "Fixedsys"},
                                             {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#0000CC},
                                             {cs, 134}],
                                      []},
                                     {text, [{c, Reply}], []}
                                    ]}),
            SendTo = if Type =:= 1 -> From; true -> To end,
            self() ! {sendpkt, protocol_helper:'msg#msg_request'(Type, SendTo, ReplyBody)},
            {noreply, State};
        %% [$\@ | TextLine] ->
        %%     Text = tl(lists:dropwhile(fun(C) -> C=/= 32 end, TextLine)),
        %%     Reply = "test reply, you said: " ++ unicode:characters_to_binary(Text),
        %%     ReplyBody = make_xml_bin(
        %%                   {msg, [], [{font, [{n, "Fixedsys"},
        %%                                      {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#EE9640},
        %%                                      {cs, 134}],
        %%                               []},
        %%                              {text, [{c, Reply}], []}
        %%                             ]}),
        %%     SendTo = if Type =:= 1 -> From; true -> To end,
        %%     self() ! {sendpkt, {sendpkt, protocol_helper:'msg#msg_request'(Type, SendTo, ReplyBody)}},
        %%     {noreply, State};
        _Other ->
            {noreply, State}
    end;

%%--------------------------------------------------------------------
%% message that no need to ack
handle_info({impacket, {{contact, _, notify, _}, [{method, "notify"}|_Params], Xml}},
            State) ->
    [{contact, [{imid, Imid}|Params], []}] = xmerl_impacket:xml_to_tuple(Xml),
    %% {Doc, _} = xmerl_scan:string(Xml),
    %% [#xmlAttribute{value=Who}] = xmerl_xpath:string("//contact/@imid", Doc),
    hi_event:contact_notify(Imid, Params),
    {noreply, State};

handle_info({impacket, {{msg, _, notify, _}, [{method, "msg_ack_notify"}|_], _}}, State) ->
    logger:log(normal, "msg:msg_ack_notify. ignore"),
    {noreply, State};

%% kickout
handle_info({impacket,{{login,_,notify,_},[{method,"kickout"}|_], _}}, State) ->
    {stop, normal, State};

%% handle other request
handle_info({impacket, {{cm, _, request, _}, [{method, "typ"}|Params], _}},
            State) ->
    {from, From} = lists:keyfind(from, 1, Params),
    hi_event:typing(From),
    self() ! {sendpkt, protocol_helper:'cm#typ'(From)},
    {noreply, State};
handle_info({impacket, {{cm, _, request, _}, [{method, "scene"}|_], _}}, State) ->
    logger:log(normal, "cm:scene request"),
    {noreply, State};
%%--------------------------------------------------------------------
%% handle ack
handle_info({impacket, {{msg,_,ack,_}, [{method, Method}|_], _}}, State) ->
    logger:log(normal, "msg:~s ack", [Method]),
    {noreply, State};

%% heartbeat move to hi_heartbeat
%% handle_info(heartbeat, State) ->
%%     logger:log(debug, "heartbeat ack!"),
%%     {noreply, State};
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
    io:format("hi_client: handle_info() ~p~n", [_Info]),
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
    hi_receiver:stop(),
    hi_heartbeat:stop(),
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

make_xml_bin(Data) ->
    Doc = xmerl:export_simple_content([Data], xmerl_xml),
    binary:list_to_bin(Doc).
