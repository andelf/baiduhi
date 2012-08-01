%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(xmerl_msg).

%% API
-export([xml_to_tuple/1]).
%% xmerl callbacks
-export(['#xml-inheritance#'/0,
         '#root#'/4,
         '#element#'/5,
         '#text#'/1]).
-export([msg/4, font/4, text/4, url/4, face/4, cface/4,
        img/4, reply/4, thumb/4]).

-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% API
%%%===================================================================
xml_to_tuple(Xml) ->
    {Doc, _} = xmerl_scan:string(Xml),
    xmerl:export([Doc], ?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%% callbacks
%% Tag := nil | {Tag, AttrsList, Data}
'#xml-inheritance#'() ->
    [].

'#text#'(_Text) ->
    "".

'#root#'(Data, _Attrs, [], _E) ->
    %% io:format("attr: ~w~n", [_Attrs]),
    Data.

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    io:format("unknown tag: ~p attr: ~w~n", [Tag, Attrs]),
    Data.

msg(Data, _Attrs, _Parents, _E) ->
    %% io:format("msg attr: ~p~n", [Attrs]),
    Data.

font(Data, _Attrs, _Parents, _E) ->
    %% io:format("font attr: ~p~n", [Attrs]),
    Data.

text(Data, Attrs, _Parents, _E) ->
    %% io:format("text attr: ~p~n", [Attrs]),
    case lists:keyfind(c, 2, Attrs) of
        #xmlAttribute{value=Text} ->
            [Data, Text];
        _Other ->
            Data
    end.

url(Data, Attrs, _Parents, _E) ->
    %% io:format("url attr: ~p~n", [Attrs]),
    case lists:keyfind(ref, 2, Attrs) of
        #xmlAttribute{value=Url} ->
            [Data, Url];
        _Other ->
            Data
    end.

face(Data, Attrs, _Parents, _E) ->
    %% io:format("face attr: ~p~n", [Attrs]),
    case lists:keyfind(n, 2, Attrs) of
        #xmlAttribute{value=Name} when length(Name) > 0 ->
            [Data, "[", Name, "]"];
        _Other ->
            Data
    end.

cface(Data, _Attrs, _Parents, _E) ->
    Name = [91,34920,24773,93],                 % [表情]
    [Data, Name].

img(Data, _Attrs, _Parents, _E) ->
    Name = [91,22270,29255,93],                 % [图片]
    [Data, Name].

reply(Data, Attrs, _Parents, _E) ->
    #xmlAttribute{value=Type} = lists:keyfind(t, 2, Attrs),
    case Type of
        "2" ->
            Name = [91,24341,29992,93];         % [引用]
        "1" ->
            Name = [91,22238,22797,93]          % [回复]
    end,
    [Data, Name].

thumb(Data, _Attrs, _Parents, _E) ->
    Data.

%%%===================================================================
%%% Internal functions
%%%===================================================================
