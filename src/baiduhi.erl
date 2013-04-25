%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(baiduhi).

%% API
%% -export([start/0, stop/0]).

%% -export([send_single_message/2, send_group_message/2, send_mchat_message/2]).
%% -export([query_contact/1, query_contacts/1, query_contact/2, query_contacts/2]).
%% -export([query_online/0]).

-compile([export_all]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:start(baiduhi),
    ok = hi_event:add_handler(hi_event_logger, []),
    ok = hi_event:add_handler(hi_event_upgrade_handler, []),
    ok = hi_event:add_handler(hi_event_handler, []).

stop() ->
    application:stop(baiduhi).


%% @spec (atom(), whatever) -> ok
%%                             {error, Code}
set_info(What, Value) ->
    set_info([{What, util:to_list(Value)}]).
set_info(Info) when is_list(Info) ->
    {ok, {{user, _, ack, _}, [{method, "set"}, {code, Code}|_], _Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'user#set'(Info)),
    case Code of
        200 ->
            ok;
        Other ->
            {error, Other}
    end.


get_info() ->
    get_info(all).

get_info(all) ->
    get_info([baiduid, birthday, email, frequency_sort, friendly_level,
              head, info_open_level, key, name, nickname, personal_comment,
              personal_desc, phone, psp_msg_count, sex, tmsg_policy, visible,
              vitality, wealth, sns_visible]);
get_info(Field) when is_atom(Field) ->
    case get_info([Field]) of
        {ok, Prop} ->
            {ok, proplists:get_value(Field, Prop)};
        Other ->
            Other
    end;
get_info(Fields) when is_list(Fields) ->
    {ok, {_, [{method, "query"}, {code, Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'user#query'(
                                          lists:map(fun atom_to_list/1,
                                                    Fields))),
    case Code of
        200 ->
            [{user, _, [{account, Params, _}]}] = util:xml_to_tuple(Xml),
            {ok, Params};
        ErrCode ->
            {error, ErrCode}
    end.


send_single_message(To, Message) ->
    send_message(1, To, Message).

send_group_message(To, Message) ->
    %% group message
    send_message(2, To, Message).

send_mchat_message(To, Message) ->
    %% multi chat message
    send_message(3, To, Message).

send_temp_message(To, Message) ->
    %% temp message
    send_message(4, To, Message).

%% contacts
query_contact(Imid) ->
    query_contact(Imid, [imid, baiduid, status, personal_comment, nickname,
                         name, email, music, cli_type, friendly_level]).
query_contact(Imid, Fields) ->
    case query_contacts([Imid], Fields) of
        {ok, [Contact]} ->
            {ok, Contact};
        Other ->
            Other
    end.

query_contacts(Imids) ->
    query_contacts(Imids, [imid, baiduid, status, personal_comment, nickname,
                           name, email, music, cli_type, friendly_level]).
query_contacts(Imids, Fields) ->
    {ok, {{contact, _, ack, _}, [{method, "query"}, {code, Code}|_], Xml}} =
        hi_client:async_impacket_requet(
          protocol_helper:'contact#query'(lists:map(fun util:to_list/1, Fields),
                                          lists:map(fun util:to_list/1, Imids))),
    case Code of
        200 ->
            [{contact_set, [], Contacts}] = util:xml_to_tuple(Xml),
            {ok, lists:map(fun({contact, Params, []}) -> Params end,
                           Contacts)};
        _Other ->
            {error, Code}
    end.

%% query_online all online id
query_online() ->
    {ok, {_, [{method, "queryonline"}, {code, Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'contact#queryonline'()),
    [{result, [{list, ImidStr}], []}] = util:xml_to_tuple(Xml),
    Imids = lists:map(fun list_to_integer/1,
                      string:tokens(ImidStr, ",")),
    case Code of
        200 ->
            {ok, Imids};
        210 ->
            query_online(Imids)
    end.
%% FIXME
query_online(Acc) ->
    {ok, {_, [{method, "queryonline"}, {code, Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'contact#queryonline'(lists:max(Acc))),
    [{result, [{list, ImidStr}], []}] = util:xml_to_tuple(Xml),
    Imids = lists:map(fun list_to_integer/1,
                      string:tokens(ImidStr, ",")),
    case Code of
        200 ->
            {ok, Acc ++ Imids};
        210 ->
            {continue, Acc ++ Imids}       %FIXME: list too long
    end.

%% friends
get_friends() ->
    {ok, {_, [{method, "get_friend"}, {code, Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'friend#get_friend'()),
    [{friend_set, [], FriendNodes}] = util:xml_to_tuple(Xml),
    FriendAttrs = lists:map(fun({friend, Attr, []}) -> Attr end,
                            FriendNodes),
    case Code of
        200 ->
            {ok, FriendAttrs};
        Other ->
            {error, Other, FriendAttrs}
    end.

get_teams() ->
    {ok, {_, [{method, "get_team"}, {code, _Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'friend#get_team'()),
    [{team_set, [], TeamNodes}] = util:xml_to_tuple(Xml),
    TeamAttrs = lists:map(fun({team, Attr, []}) -> Attr end,
                          TeamNodes),
    {ok, TeamAttrs}.

find_friend(Account) ->
    {ok, {_, [{method, "find"}, {code, Code}|Params], []}} =
         hi_client:async_impacket_requet(protocol_helper:'friend#find'(Account)),
    case Code of
        200 ->
            {imid, Imid} = lists:keyfind(imid, 1, Params),
            {ok, Imid};
        %% 210 ->
        %%     {210, Params};
        401 ->
            {imid, Imid} = lists:keyfind(imid, 1, Params),
            {not_activated, Imid};
        402 ->
            {error, "account not exists"}
    end.

%% 0	B的好友列表没有A
%% 1	B的好友列表有A,并且已经验证
%% 2	B的好友列表有A,但是未验证
query_friend_type(Imid) ->
    {ok, {_, [{method, "q_type"}, {code, Code}|Params], []}} =
        hi_client:async_impacket_requet(protocol_helper:'friend#q_type'(Imid)),
    case Code of
        200 ->
            {t, Type} = lists:keyfind(t, 1, Params),
            {ok, Type};
        _Other ->
            {error, Code}
    end.

add_friend(VerifyHeaders, Imid) ->
    add_friend(VerifyHeaders, Imid, "").
add_friend(VerifyHeaders, Imid, RequestNote) ->
    {ok, {_, [{method, "add"}, {code, Code}|_Params], []}} =
        hi_client:async_impacket_requet(protocol_helper:'friend#add'(
                                          VerifyHeaders, Imid, RequestNote)),
    case Code of
        200 ->
            {ok, Imid};
        410 ->
            {error, "verify code error"};
        444 ->
            {error, "alreay your friend"}
    end.

add_friend_reply(Agree, Imid) ->
    add_friend_reply(Agree, Imid, "").
add_friend_reply(agree, Imid, RejectReason) ->
    add_friend_reply(1, Imid, RejectReason);
add_friend_reply(reject, Imid, RejectReason) ->
    add_friend_reply(0, Imid, RejectReason);
add_friend_reply(Agree, Imid, RejectReason) when is_integer(Agree) ->
    {ok, {_, [{method, "add_ack"}, {code, Code}|_Params], []}} =
        hi_client:async_impacket_requet(protocol_helper:'friend#add_ack'(
                                          Agree, Imid, RejectReason)),
    case Code of
        200 ->
            {ok, Imid};
        _Other ->
            {error, Code}
    end.

delete_friend(VerifyHeaders, Imid) ->
    {ok, {_, [{method, "delete"}, {code, Code}|_Params], []}} =
        hi_client:async_impacket_requet(protocol_helper:'friend#delete'(VerifyHeaders, Imid)),
    case Code of
        200 ->
            {ok, Imid};
        _Other ->
            {error, Code}
    end.

%% groups
get_groups() ->
    {ok, {_, [{method, "get_list"}, {code, _Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'group#get_list'()),
    [{group_set, [], GroupNodes}] = util:xml_to_tuple(Xml),
    GroupAttrs = lists:map(fun({group, Attr, []}) -> Attr end,
                           GroupNodes),
    {ok, GroupAttrs}.

get_group(Gid) ->
    {ok, {_, [{method, "get"}, {code, Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'group#get'(Gid)),
    case Code of
        200 ->
            [{group, Attrs, [ManagerSetNode]}] = util:xml_to_tuple(Xml),
            {manager_set, [], ManagerNodes} = ManagerSetNode,
            Managers = lists:map(fun({manager,[{imid,Imid}],[]}) -> Imid end,
                                 ManagerNodes),
            {ok, Attrs, Managers};
        _Other ->
            {error, Code}
    end.

get_group_members(Gid) ->
    {ok, {_, [{method, "get_member"}, {code, Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'group#get_member'(Gid)),
    case Code of
        200 ->
            [{member_set, [], MemberNodes}] = util:xml_to_tuple(Xml),
            Members = lists:map(fun({member, [{imid,Imid}],[]}) -> Imid end,
                                MemberNodes),
            {ok, Members};
        _Other ->
            {error, Code}
    end.

security_verify(What) ->
    {ok, {_, [{method, "verify"}, {code, _Code}|_], Xml}} =
         hi_client:async_impacket_requet(protocol_helper:'security#verify'(What)),
     [{verify, VerifyHeaders, _}] = util:xml_to_tuple(Xml),
     {ok, VerifyHeaders}.
security_verify(What, Imid) ->
    {ok, {_, [{method, "verify"}, {code, _Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'security#verify'(What, Imid)),
    [{verify, VerifyHeaders, _}] = util:xml_to_tuple(Xml),
    {ok, VerifyHeaders}.

%% multi chat
create_mchat(Imids) ->
    {ok, {_, [{method, "create"}, {code, Code}|Params], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'multi#create'(Imids)),
    {mid, Mid} = lists:keyfind(mid, 1, Params),
    case Code of
        200 ->
            {ok, Mid, []};
        201 ->
            [{fail_list, [], MemberNodes}] = util:xml_to_tuple(Xml),
            FailMembers = lists:map(fun({member, Attrs, []}) -> Attrs end,
                                    MemberNodes),
            {ok, Mid, FailMembers};
        _Other ->
            {error, Code}
    end.

quit_mchat(Mid) ->
    {ok, {_, [{method, "quit"}, {code, Code}|_Params], _}} =
        hi_client:async_impacket_requet(protocol_helper:'multi#quit'(Mid)),
    case Code of
        200 ->
            ok;
        _Other ->
            {error, Code}
    end.

add_friend_to_mchat(Mid, Imid) ->
    add_friends_to_mchat(Mid, [Imid]).
add_friends_to_mchat(Mid, Imids) ->
    {ok, {_, [{method, "add"}, {code, Code}|_Params], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'multi#add'(Mid, Imids)),
    case Code of
        200 ->
            {ok, []};
        201 ->
            [{fail_list, [], MemberNodes}] = util:xml_to_tuple(Xml),
            FailMembers = lists:map(fun({member, Attrs, []}) -> Attrs end,
                                    MemberNodes),
            {ok, FailMembers};
        _Other ->
            {error, Code}
    end.

get_mchat_members(Gid) ->
    {ok, {_, [{method, "get_list"}, {code, Code}|_], Xml}} =
        hi_client:async_impacket_requet(protocol_helper:'multi#get_list'(Gid)),
    case Code of
        200 ->
            [{member_set, [], MemberNodes}] = util:xml_to_tuple(Xml),
            Members = lists:map(fun({member, [{imid,Imid}],[]}) -> Imid end,
                                MemberNodes),
            {ok, Members};
        _Other ->
                    {error, Code}
    end.


%% cm
blink(Imid) ->
    {ok, {_, [{method, "blk"}, {code, _Code}|_], _}} =
        hi_client:async_impacket_requet(protocol_helper:'cm#blk'(Imid)),
    ok.

typing(Imid) ->
    {ok, {_, [{method, "typ"}, {code, _Code}|_], _}} =
        hi_client:async_impacket_requet(protocol_helper:'cm#typ'(Imid)),
    ok.

%% only useable for baiduer. fuck.
is_baiduer(baiduid, Baiduid) ->
    {ok, Imid} = baiduhi:find_friend(Baiduid),
    is_baiduer(imid, Imid);
is_baiduer(imid, Imid) ->
    {ok, Info} = baiduhi:query_contact(Imid, [baiduer_info]),
    case proplists:get_value(baiduer_info, Info) of
        "131" ->
            true;
        _Other ->
            false
    end.

%%%===================================================================
%%% Helper functions
%%%===================================================================
baiduid_to_imid(Baiduid) ->
    baiduhi:find_friend(Baiduid).

imid_to_baiduhi(Imid) ->
    {ok, Prop} = baiduhi:query_contact(Imid, [baiduid]),
    case proplists:get_value(baiduid, Prop) of
        undefined ->
            {error, not_found};
        Baiduid ->
            {ok, Baiduid}
    end.

baiduid() ->
    hi_state:get(username).

imid() ->
    hi_state:get(imid).


%%%===================================================================
%%% Internal functions
%%%===================================================================
send_message(Type, To, Message) ->
    MessageBody = hi_msg:make_msg(Message),
    send_raw_message(Type, To, MessageBody).


send_raw_message(4, To, MessageBody) ->
    %% fuck, baiduhi doesn't use this field
    VerifyHeaders = [],
    {ok, {{msg, _, ack, _}, [{method, "tmsg_request"}, {code, Code}|_], _}} =
        hi_client:async_impacket_requet(
          protocol_helper:'msg#tmsg_request'(VerifyHeaders, To, MessageBody)),
    case Code of
        200 -> ok;
        _   -> {error, Code}
    end;
send_raw_message(Type, To, MessageBody) ->
    {ok, {{msg, _, ack, _}, [{method, "msg_request"}, {code, Code}|_], _}} =
        hi_client:async_impacket_requet(
          protocol_helper:'msg#msg_request'(Type, To, MessageBody)),
    case Code of
        200 -> ok;
        _   -> {error, Code}
    end.
