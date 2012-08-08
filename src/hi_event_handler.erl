%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(hi_event_handler).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

-export([handle_text_message/4]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({text_msg_notify, TextMessage, From, Type, ReplyTo}, State) ->
    proc_lib:spawn(?MODULE, handle_text_message, [TextMessage, From, Type, ReplyTo]),
    {ok, State};
handle_event({friend_add_notify, Imid, RequestNote}, State) ->
    case RequestNote of
        "hi" ++ _ ->
            baiduhi:add_friend_reply(true, Imid),
            {ok, VerifyHeaders} = baiduhi:security_verify(add_friend, Imid),
            baiduhi:add_friend(VerifyHeaders, Imid, "add back");
         _ ->
            baiduhi:add_friend_reply(false, Imid, "密码不告诉你")
    end,
    {ok, State};

handle_event({blink, Imid}, State) ->
    baiduhi:blink(Imid),
    {ok, State};
handle_event({typing, Imid}, State) ->
    baiduhi:typing(Imid),
    {ok, State};

handle_event(code_upgraded, State) ->
    baiduhi:set_info(status, "1;"),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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
handle_text_message(TextMessage, From, Type, ReplyTo) ->
    case TextMessage of
        "!egd " ++ Text ->
            case chart_api:make_chart({egd, Text}) of
                {ok, png, Image} ->
                    ReplyBody = util:tuple_to_xml(
                                  {msg, [], [{font, [{n, "Fixedsys"},
                                                     {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#EE9640},
                                                     {cs, 134}],
                                              []},
                                             {text, [{c, "\n"}], []},
                                             msg_fmt:img_tag({imgdata, "png", Image})
                                            ]}),
                baiduhi:send_raw_message(Type, ReplyTo, ReplyBody);
                _Other ->
                    ok
            end;
        "!qr " ++ Text ->
            case chart_api:make_chart({qr, Text}) of
                {ok, png, Image} ->
                    ReplyBody = util:tuple_to_xml(
                                  {msg, [], [{font, [{n, "Fixedsys"},
                                                     {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#EE9640},
                                                     {cs, 134}],
                                              []},
                                             {text, [{c, "\n"}], []},
                                             msg_fmt:img_tag({imgdata, "png", Image})
                                            ]});
                {error, Error} ->
                    ReplyBody = util:tuple_to_xml(
                                  {msg, [], [{font, [{n, "Fixedsys"},
                                                     {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#0000CC},
                                                     {cs, 134}],
                                              []},
                                             {text, [{c, io_lib:format("error: ~s", [Error])}], []}
                                            ]})
            end,
            baiduhi:send_raw_message(Type, ReplyTo, ReplyBody);
        "!debug" ++ _ ->
            baiduhi:set_info(has_camera, "1");
        "!status " ++ What ->
            baiduhi:set_info(personal_comment, What);
        "!echo " ++ What ->
            baiduhi:send_message(Type, ReplyTo, io_lib:format("~ts", [What]));
        "!busy " ++ What ->
            baiduhi:set_info(status, util:to_list("2;" ++ What));
        "!away " ++ What ->
            baiduhi:set_info(status, util:to_list("3;" ++ What));
        "!online" ++ _ ->
            baiduhi:set_info(status, "1;");
        "!music " ++ What ->
            case string:tokens(What, " - ") of
                [Name, Author] ->
                    baiduhi:set_info(music, "\\01" ++ "\\0" ++ util:to_list(Name) ++
                                         "\\0" ++ util:to_list(Author) ++ "\\0" ++
                                         "未知专辑" ++ "\\0");
                ["off"] ->
                    baiduhi:set_info(music, "\\00\\0\\0\\0\\0");
                _Other ->
                    baiduhi:set_info(music, "\\01" ++ "\\0" ++ util:to_list(What) ++
                                         "\\0" ++ "未知艺人" ++ "\\0" ++
                                         "未知专辑" ++ "\\0")
            end;
        "!quit mul" ++ _ ->
            if
                Type =:= 3 ->                   % if multi msg
                    baiduhi:quit_mchat(ReplyTo);
                true ->
                    ok
            end;
        "!blk" ++ _ ->
            baiduhi:blink(From);
        "!typ" ++ _ ->
            if
                Type =:= 1 ->                   % if not multi msg
                    baiduhi:typing(From);
                true ->
                    ok
            end;
        "!whoami" ->
            {ok, Info} = baiduhi:query_contact(
                           From,
                           [imid, baiduid, status, personal_comment, nickname, birthday, personal_desc,
                            name, email, music, cli_type, friendly_level, vitality, baiduer_info]),
            InfoText = lists:map(fun({Key, Value}) ->
                                         io_lib:format("~p: ~ts~n", [Key, Value])
                                 end, Info),
            baiduhi:send_message(Type, ReplyTo, InfoText);
        "!whois " ++ Who ->
            {ok, Imid} = baiduhi:find_friend(Who),
            {ok, Info} = baiduhi:query_contact(Imid,
                                               [imid, baiduid, status, personal_comment, nickname, birthday, personal_desc,
                                    name, email, music, cli_type, friendly_level, vitality, baiduer_info]),
            InfoText = lists:map(fun({Key, Value}) ->
                                         io_lib:format("~p: ~ts~n", [Key, Value])
                                 end, Info),
            baiduhi:send_message(Type, ReplyTo, InfoText);
        "!reboot " ++ Text ->
            Reply = "reboot " ++ binary_to_list(unicode:characters_to_binary(Text)) ++ " ...... ok!",
            ReplyBody = util:tuple_to_xml(
                          {msg, [], [{font, [{n, "Fixedsys"},
                                             {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#0000CC},
                                             {cs, 134}],
                                      []},
                                     {text, [{c, Reply}], []}
                                    ]}),
            baiduhi:send_raw_message(Type, ReplyTo, ReplyBody);
        "!ping" ->
            baiduhi:send_message(Type, ReplyTo, "同学您好，我是淘宝网店的客服。你寄回的充气娃娃我们已经帮你修好，马上给你寄回去。"
                                 "但请你别那么残暴的对待她，她毕竟只是个娃娃。充气娃娃寄回来时，所有在场的工作人员都落泪了~");
        _Other ->
            %% for joking
            %% baiduhi:send_message(Type, ReplyTo, "同学您好，我是淘宝网店的客服。你寄回的充气娃娃我们已经帮你修好，马上给你寄回去。"
            %% "但请你别那么残暴的对待她，她毕竟只是个娃娃。充气娃娃寄回来时，所有在场的工作人员都落泪了~"),
            ok
    end.
