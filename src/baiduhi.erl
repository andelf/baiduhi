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
    hi_event:add_handler(hi_event_logger, []).

stop() ->
    application:stop(baiduhi).


set_info(What, Value) ->
    set_info([{What, util:to_list(Value)}]).
set_info(Infos) ->
    hi_client:sendpkt_async(protocol_helper:'user#set'(Infos)),
    receive
        {impacket, {{user, _, ack, _}, [{method, "set"}, {code, Code}|_], _Xml}} ->
            case Code of
                200 ->
                    ok;
                Other ->
                    {error, Other}
            end
    end.


send_single_message(To, Message) ->
    send_msg(1, To, Message).

send_group_message(To, Message) ->
    %% group message
    send_msg(2, To, Message).

send_mchat_message(To, Message) ->
    %% group message
    send_msg(3, To, Message).

%% contacts
query_contact(Imid) ->
    query_contacts([Imid], [imid, baiduid, status, personal_comment, nickname,
                            name, email, music, cli_type, friendly_level]).
query_contact(Imid, Fields) ->
    query_contacts([Imid], Fields).
query_contacts(Imids) ->
    query_contacts(Imids, [imid, baiduid, status, personal_comment, nickname,
                              name, email, music, cli_type, friendly_level]).
query_contacts(Imids, Fields) ->
    hi_client:sendpkt_async(protocol_helper:'contact#query'(lists:map(fun util:to_list/1,
                                                                      Fields),
                                                            lists:map(fun util:to_list/1,
                                                                      Imids))),
    receive
        {impacket, {{contact, _, ack, _}, [{method, "query"}, {code, Code}|_], Xml}} ->
            io:format("code: ~p~n", [Code]),
            [{contact_set, [],
              Contacts}] = xmerl_impacket:xml_to_tuple(Xml),
            {ok, lists:map(fun({contact, Params, []}) -> Params end,
                           Contacts)}
    end.

%% query_online all online id
query_online() ->
    hi_client:sendpkt_async(protocol_helper:'contact#queryonline'()),
    receive
        {impacket, {_, [{method, "queryonline"}, {code, Code}|_], Xml}=IMPacket} ->
            [{result, [{list, ImidStr}], []}] = xmerl_impacket:xml_to_tuple(Xml),
            Imids = lists:map(fun list_to_integer/1,
                              string:tokens(ImidStr, ",")),
            io:format("~p~n", [IMPacket]),
            case Code of
                200 ->
                    {ok, Imids};
                210 ->
                    %%{ok, Imids}
                    query_online(Imids)
            end
    end.
%% FIXME
query_online(Acc) ->
    hi_client:sendpkt_async(protocol_helper:'contact#queryonline'(lists:max(Acc))),
    receive
        {impacket, {_, [{method, "queryonline"}, {code, Code}|_], Xml}=IMPacket} ->
            [{result, [{list, ImidStr}], []}] = xmerl_impacket:xml_to_tuple(Xml),
            Imids = lists:map(fun list_to_integer/1,
                              string:tokens(ImidStr, ",")),
            io:format("~p~n", [IMPacket]),
            case Code of
                200 ->
                    {ok, Acc ++ Imids};
                210 ->
                    {to_be, Acc ++ Imids}
                    %%query_online(Imids ++ Acc)
            end
    end.

%% friends
get_friends() ->
    hi_client:sendpkt_async(protocol_helper:'friend#get_friend'()),
    receive
        {impacket, {_, [{method, "get_friend"}, {code, Code}|_], Xml}=_IMPacket} ->
            [{friend_set, [], FriendNodes}] = xmerl_impacket:xml_to_tuple(Xml),
            FriendAttrs = lists:map(fun({friend, Attr, []}) -> Attr end,
                                    FriendNodes),
            case Code of
                200 ->
                    {ok, FriendAttrs};
                Other ->
                    {to_be, Other, FriendAttrs}
            end
    end.

get_teams() ->
    hi_client:sendpkt_async(protocol_helper:'friend#get_team'()),
    receive
        {impacket, {_, [{method, "get_team"}, {code, _Code}|_], Xml}=_IMPacket} ->
            [{team_set, [], TeamNodes}] = xmerl_impacket:xml_to_tuple(Xml),
            TeamAttrs = lists:map(fun({team, Attr, []}) -> Attr end,
                                  TeamNodes),
            {ok, TeamAttrs}
    end.

find_friend(Account) ->
    hi_client:sendpkt_async(protocol_helper:'friend#find'(util:to_list(Account))),
    receive
        {impacket, {_, [{method, "find"}, {code, Code}|Params], []}=_IMPacket} ->
            case Code of
                200 ->
                    {imid, Imid} = lists:keyfind(imid, 1, Params),
                    {ok, Imid};
                210 ->
                    {210, Params};
                401 ->
                    {imid, Imid} = lists:keyfind(imid, 1, Params),
                    {not_yet_activated, Imid};
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
        {impacket, {_, [{method, "q_type"}, {code, Code}|Params], []}=_IMPacket} ->
            case Code of
                200 ->
                    {t, Type} = lists:keyfind(t, 1, Params),
                    {ok, Type};
                _Other ->
                    {error, Params}
            end
    end.

%% groups
get_groups() ->
    hi_client:sendpkt_async(protocol_helper:'group#get_list'()),
    receive
        {impacket, {_, [{method, "get_list"}, {code, _Code}|_], Xml}=_IMPacket} ->
            [{group_set, [], GroupNodes}] = xmerl_impacket:xml_to_tuple(Xml),
            GroupAttrs = lists:map(fun({group, Attr, []}) -> Attr end,
                                   GroupNodes),
            {ok, GroupAttrs}
    end.

get_group(Gid) ->
    hi_client:sendpkt_async(protocol_helper:'group#get'(Gid)),
    receive
        {impacket, {_, [{method, "get"}, {code, Code}|_], Xml}=_IMPacket} ->
            case Code of
                200 ->
                    [{group, Attrs, [ManagerSetNode]}] = xmerl_impacket:xml_to_tuple(Xml),
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
        {impacket, {_, [{method, "get_member"}, {code, Code}|_], Xml}=_IMPacket} ->
            case Code of
                200 ->
                    [{member_set, [], MemberNodes}] = xmerl_impacket:xml_to_tuple(Xml),
                    Members = lists:map(fun({member, [{imid,Imid}],[]}) -> Imid end,
                                        MemberNodes),
                    {ok, Members};
                Other ->
                    {error, Other}
            end
    end.



debug_online(P) ->
    hi_client:sendpkt_async(protocol_helper:'contact#queryonline'(P)),
    receive
        {impacket, {_, [{method, "queryonline"}, {code, Code}|_], Xml}=IMPacket} ->
            [{result, [{list, ImidStr}], []}] = xmerl_impacket:xml_to_tuple(Xml),
            Imids = lists:map(fun list_to_integer/1,
                              string:tokens(ImidStr, ",")),
            io:format("~p~n", [IMPacket]),
            case Code of
                200 ->
                    {ok, Imids};
                210 ->
                    {210, Imids}
                        %%query_online(Imids)
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_msg(Type, To, Message) ->
    ReplyBody = util:make_xml_bin(
                  {msg, [], [{font, [{n, "Fixedsys"},
                                     {s, 10}, {b, 0}, {i, 0}, {ul, 0}, {c, 16#0000CC},
                                     {cs, 134}],
                              []},
                             {text, [{c, util:to_list(Message)}], []}
                            ]}),
    hi_client:sendpkt_async(protocol_helper:'msg#msg_request'(Type, To, ReplyBody)),
    receive
        {impacket, IMPacket} ->
            {ok, IMPacket}
    end.
