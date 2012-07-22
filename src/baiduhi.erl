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

send_single_message(To, Message) ->
    send_msg(1, To, Message).

send_group_message(To, Message) ->
    %% group message
    send_msg(2, To, Message).

send_mchat_message(To, Message) ->
    %% group message
    send_msg(3, To, Message).


query_contact(Imid) ->
    query_contacts([Imid], [imid, baiduid, status, personal_comment, nickname,
                            name, email, music, cli_type]).
query_contact(Imid, Fields) ->
    query_contacts([Imid], Fields).
query_contacts(Imids) ->
    query_contacts(Imids, [imid, baiduid, status, personal_comment, nickname,
                              name, email, music, cli_type]).
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
