%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(hi_event_logger).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

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
handle_event(user_login_ready, State) ->
    error_logger:info_msg("login ready: username=~p id=~p", [hi_state:get(username),
                                                             hi_state:uid()]),
    {ok, State};
handle_event({contact_notify, Imid, Params}, State) ->
    error_logger:info_msg("contact:notify <imid:~s> ~180p", [Imid, Params]),
    {ok, State};
handle_event({msg_notify, _Message, _From, _Type, _ReplyTo}, State) ->
    %% ConvertType = fun(1) -> "SINGLE";
    %%                  (2) -> "GROUP";
    %%                  (3) -> "MCHAT"
    %%               end,
    %% error_logger:info_msg("~s full msg <from:~p> <replyto:~p>:~n~100p~n", [ConvertType(Type), From, ReplyTo, Message]),
    {ok, State};
handle_event({text_msg_notify, Text, From, Type, ReplyTo}, State) ->
    ConvertType = fun(1) -> "SINGLE";
                     (2) -> "GROUP";
                     (3) -> "MCHAT";
                     (4) -> "TEMP"
                  end,
    error_logger:info_msg("~s text msg <from:~p> <replyto:~p>: ~ts", [ConvertType(Type), From, ReplyTo, Text]),
    {ok, State};
handle_event({friend_add_notify, Imid, RequestNote}, State) ->
    error_logger:info_msg("add friend request <from:~p>: ~p", [Imid, RequestNote]),
    {ok, State};

handle_event({blink, Imid}, State) ->
    error_logger:info_msg("blink notify <from:~p>", [Imid]),
    {ok, State};

handle_event({typing, Imid}, State) ->
    error_logger:info_msg("typing notify <from:~p>", [Imid]),
    {ok, State};

handle_event(code_upgraded, State) ->
    error_logger:info_msg("code upgraded!", []),
    {ok, State};

handle_event({group_add_member_notify, Gid, Manager, Imids}, State) ->
    error_logger:info_msg("~p being added to <gid:~p> by <imid:~p>", [Imids, Gid, Manager]),
    {ok, State};

handle_event({group_delete_member_notify, Gid, Manager, Imids}, State) ->
    error_logger:info_msg("~p being deleted to <gid:~p> by <imid:~p>", [Imids, Gid, Manager]),
    {ok, State};

handle_event(Event, State) ->
    error_logger:info_msg("unhandled log ~p", [Event]),
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
