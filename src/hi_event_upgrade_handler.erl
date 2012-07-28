%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%% erlang不能通过code:load_file/1更新本身, 所以把!upgrade消息处理单独拿出来
%%% @end
%%% Created : 17 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(hi_event_upgrade_handler).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0, reload_code/1]).

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
handle_event({text_msg, TextMessage, From, Type, ReplyTo}, State) ->
    case TextMessage of
        "!upgrade" ->
            baiduhi:set_info(status, "2;IM Robot System Upgrading ..."),
            case From of
                406526983 ->
                    MsgCbFun = fun(Msg) -> baiduhi:send_message(Type, ReplyTo, Msg) end,
                    proc_lib:spawn(?MODULE, reload_code, [MsgCbFun]);
                _Other ->
                    baiduhi:send_message(Type, ReplyTo, "You are not Administrator!")
            end;
        _Other ->
            ok
    end,
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
reload_code(MessageCallbackFun) ->
    GitUpdateMessage = os:cmd("git pull origin master"),
    MessageCallbackFun(GitUpdateMessage),
    RebarOutput = lists:map(
                    fun(Line) ->
                            io:format("Line: ~s~n", [Line]),
                            case Line of
                                "Compiled src/" ++ FileName ->
                                    ModName = list_to_atom(lists:sublist(FileName, length(FileName) - 4)),
                                    {mod, ModName};
                                "ERROR: " ++ ErrorMessage ->
                                    {error, ErrorMessage};
                                Other ->
                                    {ignore, Other}
                            end
                    end, string:tokens(os:cmd("./rebar compile"), "\n")),
    case lists:keyfind(error, 1, RebarOutput) of
        {error, ErrorMessage} ->
            MessageCallbackFun(ErrorMessage);
        _Other ->
            ResultMessage = lists:foldl(
                              fun({mod, Mod}, AccIn) ->
                                      case code:load_file(Mod) of
                                          {module, Mod} ->
                                              io:format("~sload ~p ok~n", [AccIn, Mod]),
                                              io_lib:format("~sload ~p ok~n", [AccIn, Mod]);
                                          {error, Error} ->
                                              io:format("~sload ~p error: ~p~n", [AccIn, Mod, Error]),
                                              io_lib:format("~sload ~p error: ~p~n", [AccIn, Mod, Error])
                                      end;
                                 (_, AccIn) ->
                                      AccIn
                              end, [], RebarOutput),
            MessageCallbackFun(ResultMessage)
    end,
    hi_event:add_handler(hi_event_handler, []),
    hi_event:code_upgraded().
