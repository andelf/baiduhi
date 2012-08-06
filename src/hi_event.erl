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

-export([user_login_ready/0, contact_notify/2, text_msg_notify/4, msg_notify/4]).
%% friend
-export([friend_add_notify/2, friend_change/0]).
%% group
-export([group_add_member_notify/3, group_delete_member_notify/3]).
%% cm
-export([blink/1, typing/1]).
%% custum
-export([code_upgraded/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_sup_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%% API functions

%% 约定, 一般消息名为 Command_Method
user_login_ready() ->
    gen_event:notify(?SERVER, user_login_ready).

contact_notify(Imid, What) ->
    gen_event:notify(?SERVER, {contact_notify, Imid, What}).

%% raw message
msg_notify(Message, From, Type, ReplyTo) ->
    gen_event:notify(?SERVER, {msg_notify, Message, From, Type, ReplyTo}).

text_msg_notify(Text, From, Type, ReplyTo) ->
    gen_event:notify(?SERVER, {text_msg_notify, Text, From, Type, ReplyTo}).

friend_add_notify(Imid, RequestNote) ->
    gen_event:notify(?SERVER, {friend_add_notify, Imid, RequestNote}).

friend_change() ->
    gen_event:notify(?SERVER, {friend_change, "Something"}).

%% group
group_add_member_notify(Gid, Manager, Imids) ->
    gen_event:notify(?SERVER, {group_add_member_notify, Gid, Manager, Imids}).
group_delete_member_notify(Gid, Manager, Imids) ->
    gen_event:notify(?SERVER, {group_delete_member_notify, Gid, Manager, Imids}).
%% cm event name
blink(Imid) ->
    gen_event:notify(?SERVER, {blink, Imid}).

typing(Imid) ->
    gen_event:notify(?SERVER, {typing, Imid}).

code_upgraded() ->
    gen_event:notify(?SERVER, code_upgraded).

%%%===================================================================
%%% Internal functions
%%%===================================================================
