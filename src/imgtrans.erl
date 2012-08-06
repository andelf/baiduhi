%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created :  5 Aug 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(imgtrans).

-compile([export_all]).
%% API
%-export([]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_server_info() ->
    hi_client:sendpkt_async(protocol_helper:imagesvr()),
    receive
        {ack, {{imagesvr, _, ack, _}, [{code, Code}|_], ServerInfo}} ->
            {Token, IP, Port} = protocol_helper:parse_imagesvr(ServerInfo),
            {Token, IP, Port}
    end.




%%%===================================================================
%%% Internal functions
%%%===================================================================
