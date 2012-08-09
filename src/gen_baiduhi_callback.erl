%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%% gen_baiduhi_callback behaviour
%%% @end
%%% Created :  9 Aug 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(gen_baiduhi_callback).

%% API
-export([start_link/3]).
-export([behaviour_info/1]).

%%%===================================================================
%%% API
%%%===================================================================

behaviour_info(callbacks) ->
    [];
behaviour_info(_Other) ->
    undefined.

start_link(Callback, Priority, Args) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
