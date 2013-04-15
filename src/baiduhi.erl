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
    hi_event:add_handler(hi_event_logger, []),
    hi_event:add_handler(hi_event_upgrade_handler, []),
    hi_event:add_handler(hi_event_handler, []).

stop() ->
    application:stop(baiduhi).


%% @spec (atom(), whatever) -> ok
%%                             {error, Code}
set_info(What, Value) ->
    set_info([{What, util:to_list(Value)}]).
set_info(Info) when is_list(Info) ->
    hi_client:sendpkt_async(protocol_helper:'user#set'(Info)),
    receive
        {ack, {{user, _, ack, _}, [{method, "set"}, {code, Code}|_], _Xml}} ->
            case Code of
                200 ->
                    ok;
                Other ->
                    {error, Other}
            end
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
    hi_client:sendpkt_async(protocol_helper:'user#query'(
                              lists:map(fun atom_to_list/1,
                                        Fields))),
    receive
        {ack, {_, [{method, "query"}, {code, Code}|_], Xml}} ->
            case Code of
                200 ->
                    [{user, _, [{account, Params, _}]}] = util:xml_to_tuple(Xml),
                    {ok, Params};
                ErrCode ->
                    {error, ErrCode}
            end
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
    hi_client:sendpkt_async(
      protocol_helper:'contact#query'(lists:map(fun util:to_list/1, Fields),
                                      lists:map(fun util:to_list/1, Imids))),
    receive
        {ack, {{contact, _, ack, _}, [{method, "query"}, {code, Code}|_], Xml}} ->
            case Code of
                200 ->
                    [{contact_set, [], Contacts}] = util:xml_to_tuple(Xml),
                    {ok, lists:map(fun({contact, Params, []}) -> Params end,
                                   Contacts)};
                _Other ->
                    {error, Code}
            end
    end.

%% query_online all online id
query_online() ->
    hi_client:sendpkt_async(protocol_helper:'contact#queryonline'()),
    receive
        {ack, {_, [{method, "queryonline"}, {code, Code}|_], Xml}=_IMPacket} ->
            [{result, [{list, ImidStr}], []}] = util:xml_to_tuple(Xml),
            Imids = lists:map(fun list_to_integer/1,
                              string:tokens(ImidStr, ",")),
            case Code of
                200 ->
                    {ok, Imids};
                210 ->
                    query_online(Imids)
            end
    end.
%% FIXME
query_online(Acc) ->
    hi_client:sendpkt_async(protocol_helper:'contact#queryonline'(lists:max(Acc))),
    receive
        {ack, {_, [{method, "queryonline"}, {code, Code}|_], Xml}=_IMPacket} ->
            [{result, [{list, ImidStr}], []}] = util:xml_to_tuple(Xml),
            Imids = lists:map(fun list_to_integer/1,
                              string:tokens(ImidStr, ",")),
            case Code of
                200 ->
                    {ok, Acc ++ Imids};
                210 ->
                    {continue, Acc ++ Imids}       %FIXME: list too long
            end
    end.

%% friends
get_friends() ->
    hi_client:sendpkt_async(protocol_helper:'friend#get_friend'()),
    receive
        {ack, {_, [{method, "get_friend"}, {code, Code}|_], Xml}=_IMPacket} ->
            [{friend_set, [], FriendNodes}] = util:xml_to_tuple(Xml),
            FriendAttrs = lists:map(fun({friend, Attr, []}) -> Attr end,
                                    FriendNodes),
            case Code of
                200 ->
                    {ok, FriendAttrs};
                Other ->
                    {error, Other, FriendAttrs}
            end
    end.

get_teams() ->
    hi_client:sendpkt_async(protocol_helper:'friend#get_team'()),
    receive
        {ack, {_, [{method, "get_team"}, {code, _Code}|_], Xml}=_IMPacket} ->
            [{team_set, [], TeamNodes}] = util:xml_to_tuple(Xml),
            TeamAttrs = lists:map(fun({team, Attr, []}) -> Attr end,
                                  TeamNodes),
            {ok, TeamAttrs}
    end.

find_friend(Account) ->
    hi_client:sendpkt_async(protocol_helper:'friend#find'(util:to_list(Account))),
    receive
        {ack, {_, [{method, "find"}, {code, Code}|Params], []}=_IMPacket} ->
            case Code of
                200 ->
                    {imid, Imid} = lists:keyfind(imid, 1, Params),
                    {ok, Imid};
                210 ->
                    {210, Params};
                401 ->
                    {imid, Imid} = lists:keyfind(imid, 1, Params),
                    {not_activated, Imid};
                402 ->
                    {error, "account not exists"}
            end
    end.

%% 0	B的好友列表没有A
%% 1	B的好友列表有A,并且已经验证
%% 2	B的好友列表有A,但是未验证
query_friend_type(Imid) ->
    hi_client:sendpkt_async(protocol_helper:'friend#q_type'(Imid)),
    receive
        {ack, {_, [{method, "q_type"}, {code, Code}|Params], []}=_IMPacket} ->
            case Code of
                200 ->
                    {t, Type} = lists:keyfind(t, 1, Params),
                    {ok, Type};
                _Other ->
                    {error, Params}
            end
    end.


add_friend(VerifyHeaders, Imid) ->
    add_friend(VerifyHeaders, Imid, "").
add_friend(VerifyHeaders, Imid, RequestNote) ->
    hi_client:sendpkt_async(protocol_helper:'friend#add'(VerifyHeaders, Imid, RequestNote)),
    receive
        {ack, {_, [{method, "add"}, {code, Code}|_Params], []}=_IMPacket} ->
            case Code of
                200 ->
                    {ok, Imid};
                410 ->
                    {error, "verify code error"};
                444 ->
                    {error, "alreay your friend"}
            end
    end.

add_friend_reply(Agree, Imid) ->
    add_friend_reply(Agree, Imid, "").
add_friend_reply(agree, Imid, RejectReason) ->
    add_friend_reply(1, Imid, RejectReason);
add_friend_reply(reject, Imid, RejectReason) ->
    add_friend_reply(0, Imid, RejectReason);
add_friend_reply(Agree, Imid, RejectReason) when is_integer(Agree) ->
    hi_client:sendpkt_async(protocol_helper:'friend#add_ack'(Agree, Imid, RejectReason)),
    receive
        {ack, {_, [{method, "add_ack"}, {code, Code}|_Params], []}=_IMPacket} ->
            case Code of
                200 ->
                    {ok, Imid};
                Other ->
                    {error, Other}
            end
    end.

delete_friend(VerifyHeaders, Imid) ->
    hi_client:sendpkt_async(protocol_helper:'friend#delete'(VerifyHeaders, Imid)),
    receive
        {ack, {_, [{method, "delete"}, {code, Code}|_Params], []}=_IMPacket} ->
            case Code of
                200 ->
                    {ok, Imid};
                Other ->
                    {error, Other}
            end
    end.

%% groups
get_groups() ->
    hi_client:sendpkt_async(protocol_helper:'group#get_list'()),
    receive
        {ack, {_, [{method, "get_list"}, {code, _Code}|_], Xml}=_IMPacket} ->
            [{group_set, [], GroupNodes}] = util:xml_to_tuple(Xml),
            GroupAttrs = lists:map(fun({group, Attr, []}) -> Attr end,
                                   GroupNodes),
            {ok, GroupAttrs}
    end.

get_group(Gid) ->
    hi_client:sendpkt_async(protocol_helper:'group#get'(Gid)),
    receive
        {ack, {_, [{method, "get"}, {code, Code}|_], Xml}=_IMPacket} ->
            case Code of
                200 ->
                    [{group, Attrs, [ManagerSetNode]}] = util:xml_to_tuple(Xml),
                    {manager_set, [], ManagerNodes} = ManagerSetNode,
                    Managers = lists:map(fun({manager,[{imid,Imid}],[]}) -> Imid end,
                                         ManagerNodes),
                    {ok, Attrs, Managers};
                Other ->
                    {error, Other}
            end
    end.

get_group_members(Gid) ->
    hi_client:sendpkt_async(protocol_helper:'group#get_member'(Gid)),
    receive
        {ack, {_, [{method, "get_member"}, {code, Code}|_], Xml}=_IMPacket} ->
            case Code of
                200 ->
                    [{member_set, [], MemberNodes}] = util:xml_to_tuple(Xml),
                    Members = lists:map(fun({member, [{imid,Imid}],[]}) -> Imid end,
                                        MemberNodes),
                    {ok, Members};
                Other ->
                    {error, Other}
            end
    end.

security_verify(What) ->
    hi_client:sendpkt_async(protocol_helper:'security#verify'(What)),
    receive
        {ack, {_, [{method, "verify"}, {code, _Code}|_], Xml}=_IMPacket} ->
            [{verify, VerifyHeaders, _}] = util:xml_to_tuple(Xml),
            {ok, VerifyHeaders}
    end.
security_verify(What, Imid) ->
    hi_client:sendpkt_async(protocol_helper:'security#verify'(What, Imid)),
    receive
        {ack, {_, [{method, "verify"}, {code, _Code}|_], Xml}=_IMPacket} ->
            [{verify, VerifyHeaders, _}] = util:xml_to_tuple(Xml),
            {ok, VerifyHeaders}
    end.


%% multi
create_mchat(Imids) ->
    hi_client:sendpkt_async(protocol_helper:'multi#create'(lists:map(fun util:to_list/1,
                                                                     Imids))),
    receive
        {ack, {_, [{method, "create"}, {code, Code}|Params], Xml}=_IMPacket} ->
            {mid, Mid} = lists:keyfind(mid, 1, Params),
            case Code of
                200 ->
                    {ok, Mid, []};
                201 ->
                    [{fail_list, [], MemberNodes}] = util:xml_to_tuple(Xml),
                    FailMembers = lists:map(fun({member, Attrs, []}) -> Attrs end,
                                            MemberNodes),
                    {ok, Mid, FailMembers};
                Other ->
                    {error, Other, _IMPacket}
            end
    end.

quit_mchat(Mid) ->
    hi_client:sendpkt_async(protocol_helper:'multi#quit'(Mid)),
    receive
        {ack, {_, [{method, "quit"}, {code, Code}|_Params], _}=_IMPacket} ->
            case Code of
                200 ->
                    ok;
                Other ->
                    {error, Other}
            end
    end.

add_friend_to_mchat(Mid, Imid) ->
    add_friends_to_mchat(Mid, [Imid]).
add_friends_to_mchat(Mid, Imids) ->
    hi_client:sendpkt_async(protocol_helper:'multi#add'(Mid, lists:map(fun util:to_list/1, Imids))),
    receive
        {ack, {_, [{method, "add"}, {code, Code}|_Params], Xml}=_IMPacket} ->
            case Code of
                200 ->
                    {ok, []};
                201 ->
                    [{fail_list, [], MemberNodes}] = util:xml_to_tuple(Xml),
                    FailMembers = lists:map(fun({member, Attrs, []}) -> Attrs end,
                                            MemberNodes),
                    {ok, FailMembers};
                Other ->
                    {error, Other}
            end
    end.

get_mchat_members(Gid) ->
    hi_client:sendpkt_async(protocol_helper:'multi#get_list'(Gid)),
    receive
        {ack, {_, [{method, "get_list"}, {code, Code}|_], Xml}=_IMPacket} ->
            case Code of
                200 ->
                    [{member_set, [], MemberNodes}] = util:xml_to_tuple(Xml),
                    Members = lists:map(fun({member, [{imid,Imid}],[]}) -> Imid end,
                                        MemberNodes),
                    {ok, Members};
                Other ->
                    {error, Other}
            end
    end.


%% cm
blink(Imid) ->
    hi_client:sendpkt_async(protocol_helper:'cm#blk'(Imid)),
    receive
        {ack, {_, [{method, "blk"}, {code, _Code}|_], _}=_IMPacket} ->
            {ok, Imid}
    end.

typing(Imid) ->
    hi_client:sendpkt_async(protocol_helper:'cm#typ'(Imid)),
    receive
        {ack, {_, [{method, "typ"}, {code, _Code}|_], _}=_IMPacket} ->
            {ok, Imid}
    end.

debug_online(P) ->
    hi_client:sendpkt_async(protocol_helper:'contact#queryonline'(P)),
    receive
        {ack, {_, [{method, "queryonline"}, {code, Code}|_], Xml}=_IMPacket} ->
            [{result, [{list, ImidStr}], []}] = util:xml_to_tuple(Xml),
            Imids = lists:map(fun list_to_integer/1,
                              string:tokens(ImidStr, ",")),
            case Code of
                200 ->
                    {ok, Imids};
                210 ->
                    {210, Imids}
                        %%query_online(Imids)
            end
    end.

%% only useable for baiduer. fuck.
is_baiduer(baiduid, Baiduid) ->
    {ok, Imid} = baiduhi:find_friend(Baiduid),
    is_baiduer(imid, Imid);
is_baiduer(imid, Imid) ->
    {ok, Info} = baiduhi:query_contact(Imid, [baiduer_info]),
    case lists:keyfind(baiduer_info, 1, Info) of
        {baiduer_info, "131"} ->
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
    MessageBody = util:tuple_to_xml(
                    {msg, [], [{font, [{n, "Fixedsys"},
                                       {s, 10}, {b, 0}, {i, 0},
                                       {ul, 0}, {c, 16#0000CC},
                                       {cs, 134}],
                                []},
                               {text, [{c, util:to_list(Message)}], []}
                              ]}),
    send_raw_message(Type, To, MessageBody).


send_raw_message(4, To, MessageBody) ->
    %% fuck, baiduhi doesn't use this field
    VerifyHeaders = [],
    hi_client:sendpkt_async(protocol_helper:'msg#tmsg_request'(VerifyHeaders, To, MessageBody)),
    receive
        {ack, {{msg, _, ack, _}, [{method, "tmsg_request"}, {code, Code}|_], _Xml}} ->
            case Code of
                200 -> ok;
                Other -> {error, Other}
            end
    end;
send_raw_message(Type, To, MessageBody) ->
    hi_client:sendpkt_async(protocol_helper:'msg#msg_request'(Type, To, MessageBody)),
    receive
        {ack, {{msg, _, ack, _}, [{method, "msg_request"}, {code, Code}|_], _}} ->
            case Code of
                200 -> ok;
                Other -> {error, Other}
            end
    end.
