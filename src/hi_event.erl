%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(hi_event).

%% API
-export([start_link/0, add_handler/2, delete_handler/2]).

-export([login_ready/0]).
-export([xxxx/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%% API functions
xxxx(What) ->
    gen_event:notify(?SERVER, {xxx, What}).

login_ready() ->
    gen_event:notify(?SERVER, login_ready).

%%%===================================================================
%%% Internal functions
%%%===================================================================
