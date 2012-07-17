%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created :  3 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(baiduhi_config).

%% API
-export([get_env/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_env(logger_logdir) ->
    {ok, "./"};
get_env(logger_logbasename) ->
    %%{ok, "/media/winE/Works/erlang/baiduhi/log/hi"};
    %% {ok, "E:/Works/erlang/baiduhi/log/hi"};
    application:get_env(baiduhi, logger_logbasename);
get_env(logger_status) ->
    {ok, [{all, true}]};
get_env(max_logfile_size) ->
    {ok, 0}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
