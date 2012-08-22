%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(noah).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init_noah/0]).

-export([search_nodes/1, get_children_nodes/0, get_children_nodes/1, get_node_by_name/1]).
-export([search_monitor_items/2, search_hosts/1, search_hosts/2, get_monitor_rule_status_by_host/1]).
-export([search_ip/1, get_host_ownership/1, get_host_mounted_nodes/1]).
-export([search_baiduer/1, init_family/0]).

-export([debug/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-record(host,{id, name}).
-record(monitor_item,{id, name, description, unit}).
-record(node,{id, path, is_leaf}).
-record(monitor_rule_status, {id, description, is_blocked,
                              restore_time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}, 0}.

init_noah() ->
    httpc:request("http://noah.baidu.com/olive/index.php?r=Passport/Logging/Index", ?MODULE).

init_family() ->
    httpc:request("http://family.baidu.com/", ?MODULE),
    httpc:request("http://family.baidu.com/core/index.jsp", ?MODULE),
    httpc:request("http://family.baidu.com/core/index.jsp?chc=1603971430", ?MODULE).

search_baiduer(Keyword) ->
    Raw = http_get("http://family.baidu.com/addressbook/searchuser.do",
                   [{pagesize, 30}, {pageNum, 1}, {isByStart, 0},
                    {s, Keyword}],
                   ["result"]),
    lists:map(fun(JSON) ->
                      {baiduer,
                       get_string_field(JSON, "username"),
                       get_string_field(JSON, "name"),
                       get_string_field(JSON, "displayName"),
                       get_string_field(JSON, "hi"),
                       get_string_field(JSON, "email"),
                       get_integer_field(JSON, "phone"),
                       get_integer_field(JSON, "mobile"),
                       get_string_field(JSON, "dept")}
              end, Raw).

%% -----------------------------
%% 规范:
%% search_xxx() -> [XXX] 仅名字
%% get_xxx_by_name(Name) -> [{xxx, ...}]
%% -----------------------------
search_nodes(Keyword) ->
    Raw = http_get("http://noah.baidu.com/olive/?r=Apply/Privilege/SearchNodes",
                   [{key, Keyword}, {type, "node"}],
                   ["data"]),
    %% 处理: <未找到>
    lists:filter(
      fun(Name) -> Name =/= [60,230,156,170,230,137,190,229,136,176,62] end,
      lists:map(fun binary_to_list/1, Raw)).

search_hosts(Keyword) ->
    Raw = http_get("http://noah.baidu.com/goat/index.php?r=Host/Host/GetGlobalHost",
                   [{curPage, 1}, {perPage, 20}, {order, ""}, {token, Keyword}],
                   ["data", "list"]),
    lists:map(fun(JSON) ->
                      {host,
                       get_integer_field(JSON, "hostId"),
                       get_string_field(JSON, "name")}
              end, Raw).
search_hosts(NodeId, Keyword) ->
    Raw = http_get("http://noah.baidu.com/noah/index.php?r=block/list",
                   [{list, "host"}, {nodeid, NodeId}, {perPage, 20},
                    {curPage, 1}, {token, Keyword}],
                   ["data", "data"]),
    case Raw of
        {obj, RawList} ->
            lists:map(fun({StringId, BinaryName}) ->
                              {host,
                               list_to_integer(StringId),
                               binary_to_list(BinaryName)}
                      end, RawList);
        _Other ->
            []
    end.

search_monitor_items(NodeId, Keyword) ->
    Raw = http_get("http://noah.baidu.com/noah/?r=monitorItem/ListDataTypeItem",
                   [{token, Keyword}, {node_id, NodeId}],
                   []),
    lists:map(fun(JSON) ->
                      {monitor_item,
                       get_integer_field(JSON, "item_id"),
                       get_string_field(JSON, "item_name"),
                       get_string_field(JSON, "item_alias"),
                       get_string_field(JSON, "item_unit")}
              end, Raw).

search_ip(Keyword) ->
    Raw = http_get("http://noah.baidu.com/goat/index.php?r=Host/Host/GetIPBelongs",
                   [{curPage,1},{perPage,255},{order,""},{token,Keyword}],
                   ["data", "list"]),
    lists:map(fun(JSON) ->
                      {ip,
                       get_string_field(JSON, "ip"),
                       get_string_field(JSON, "idc_name"),
                       get_string_field(JSON, "service_path")}
              end, Raw).

get_children_nodes() ->
    get_children_nodes(1).
get_children_nodes(NodeId) ->
    Raw = http_get("http://noah.baidu.com/olive/?r=Tree/Info/GetChildrenNode",
                   [{treeId, 1}, {nodeId, NodeId}],
                   []),
    lists:map(fun(JSON) ->
                      {node,
                       get_integer_field(JSON, "id"),
                       get_string_field(JSON, "path"),
                       rfc4627:get_field(JSON, "leaf", true)}
              end, Raw).

get_node_by_name(Name) ->
    [UnifiedName] = search_nodes(Name),
    get_node_by_name(UnifiedName, 1).
get_node_by_name(Name, CurrendNodeId) ->
    ChildrenNodes = get_children_nodes(CurrendNodeId),
    find_node_in_children_nodes(Name, ChildrenNodes).

%% only 1 node
get_host_ownership(HostId) ->
    Raw = http_get("http://noah.baidu.com/goat/index.php?r=Host/Host/GetAssetBelongs",
                   [{hostId, HostId}],
                   ["data", "list"]),
    lists:map(fun(JSON) ->
                      {node,
                       get_integer_field(JSON, "id"),
                       get_string_field(JSON, "path"),
                       false}
              end, Raw).

get_host_mounted_nodes(HostId) ->
    Raw = http_get("http://noah.baidu.com/goat/index.php?r=Host/Host/GetHangOn",
                   [{curPage,1},{hostId, HostId}],
                   ["data", "list"]),
    lists:map(fun binary_to_list/1, Raw).

get_monitor_rule_status_by_host(HostId) ->
    Raw = http_get("http://noah.baidu.com/noah/index.php?r=block/viewByHost",
                   [{hostId, HostId}],
                   ["data"]),
    lists:map(fun(JSON) ->
                      {ok, Rule} = rfc4627:get_field(JSON, "rule"),
                      {monitor_rule_status,
                       get_integer_field(Rule, "ruleId"),
                       get_string_field(Rule, "note"),
                       case get_integer_field(JSON, "block") of
                           1 -> true;
                           _Other -> false
                       end,
                       get_string_field(JSON, "restoreTime")}
              end, Raw).



%% monitor data

debug() ->
    RawText = gen_server:call(?MODULE, {http_request, get, "http://noah.baidu.com/olive/?r=Apply/Privilege/SearchNodes",
                                     [{key, "appac"}, {type, "node"}]}),
    {ok, JSON, []} = rfc4627:decode(RawText),
    case extract_json_value(JSON, ["data"]) of
        not_found ->
            [];
        Data ->
            lists:map(fun binary_to_list/1, Data)
    end.

http_get(Url, Params, ReturnPath) ->
    RawText = gen_server:call(?MODULE, {http_request, get, Url, Params}),
    %%io:format("~p~n",[RawText]),
    {ok, JSON, []} = rfc4627:decode(RawText),
    case extract_json_value(JSON, ReturnPath) of
        not_found ->
            [];
        Data ->
            Data
    end.


extract_json_value(not_found, _) ->
    not_found;
extract_json_value(JSON, []) ->
    JSON;
extract_json_value(JSON, [Key|Remind]) ->
    extract_json_value(rfc4627:get_field(JSON, Key, not_found), Remind).

get_integer_field(JSON, Key) ->
    {ok, BinVal} = rfc4627:get_field(JSON, Key),
    util:to_integer(BinVal).

get_string_field(JSON, Key) ->
    {ok, BinVal} = rfc4627:get_field(JSON, Key),
    util:to_list(BinVal).




%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({http_request, Method, Url, Params}, _From, State) ->
    case Method of
        get ->
            QueryString = util:url_encode(Params),
            GetUrl = case string:str(Url, "?") of
                         0 ->
                             Url ++ "?" ++ QueryString;
                         _Other ->
                             Url ++ "&" ++ QueryString
                     end,

            {ok, {_, _, Body}} = httpc:request(get, {GetUrl, []}, % 时间 ms 为单位
                                               [{timeout, 10000}, {connect_timeout, 10000}],
                                               [], ?MODULE);
        post ->
            {ok, {_, _, Body}} = httpc:request(post, {Url, [], "application/x-www-form-urlencoded", util:url_encode(Params)},
                                               [],
                                               [], ?MODULE)
    end,
    Reply = Body,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    ConfFile = filename:join(code:priv_dir(baiduhi), "baiduhi.conf"),
    {ok, [{baiduhi, Conf}]} = file:consult(ConfFile),
    {noah_username, Username} = lists:keyfind(noah_username, 1, Conf),
    {noah_password, Password} = lists:keyfind(noah_password, 1, Conf),
    inets:start(),
    ssl:start(),                                % https support
    inets:start(httpc, [{profile, ?MODULE}]),
    httpc:set_options([{cookies, enabled}], ?MODULE),
    Url = "https://uuap.baidu.com/login",
    {ok, {_,_,Body}} = httpc:request(Url, ?MODULE),
    case string:str(Body, "You have successfully logged into") of
        0 ->
            {ok, Action, LT, Execution} = extract_params_from_uuap_page(Body),
            Params = [{"username", Username},
                      {"password", Password},
                      {"_rememberMe", "on"},
                      {"_viaToken", "on"},
                      {"lt", LT},
                      {"execution", Execution},
                      {"_eventId", "submit"}],
            PostUrl = "https://uuap.baidu.com" ++ Action,
            {ok, {_, _, Body2}} = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", util:url_encode(Params)},
                                               [],
                                               [{body_format, string}], ?MODULE),
            case string:str(Body2, "Log In Successful") of
                0 ->
                    {stop, login_failed, State};
                _Other ->
                    {noreply, State}
            end;
        _Other ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
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
extract_params_from_uuap_page(Body) ->
    {match,[[Action]]} = re:run(Body, "action=\"(/login[^\"]*)", [global,{capture,[1],list}]),
    {match,[[LT]]} = re:run(Body, "name=\"lt\"\s+value=\"([^\"]*)\"", [global,{capture,[1],list}]),
    {match,[[Execution]]} = re:run(Body, "name=\"execution\"\s+value=\"([^\"]*)\"", [global,{capture,[1],list}]),
    {ok, Action, LT, Execution}.

find_node_in_children_nodes(Name, [Node={node, _Id, Name, _IsLeaf}|_Acc]) ->
    Node;
find_node_in_children_nodes(Name, [{node, Id, Path, _IsLeaf}|Acc]) ->
    NamePrefix = lists:sublist(Name, length(Path)+1),
    PathPrefix = Path ++ "_",
    if
        NamePrefix =:= PathPrefix ->
            find_node_in_children_nodes(Name, get_children_nodes(Id));
        true ->
            find_node_in_children_nodes(Name, Acc)
    end;
find_node_in_children_nodes(_Name, []) ->
    not_found.
