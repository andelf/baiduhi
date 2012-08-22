%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 18 Aug 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(noah_deploy).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([parse_archer_page/1,
        parse_prado_page/1]).

-export([extract_info_from_archer_page/1,
         extract_info_from_prado_page/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

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

parse_archer_page(Id) ->
    %% timeout 5s
    gen_server:call(?SERVER, {parse_archer_page, Id}, 5000).

parse_prado_page(Id) ->
    %% timeout 5s
    gen_server:call(?SERVER, {parse_prado_page, Id}, 5000).

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
handle_call({parse_archer_page, Id}, _From, State) ->
    {ok, Body} = http_request(get,
                              "http://api.noah.baidu.com/ci-web/index.php?r=ProcessView/QueryTask",
                              [{listid, Id}]),
    Reply = extract_info_from_archer_page(Body),
    {reply, Reply, State};
handle_call({parse_prado_page, Id}, _From, State) ->
    {ok, Body} = http_request(get,
                              "http://noah.baidu.com/deploy/prado/index.php?page=Deploy.ViewDeployProcess",
                              [{id, Id}]),
    Reply = extract_info_from_prado_page(Body),
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
    httpc:request("http://noah.baidu.com/olive/index.php?r=Passport/Logging/Index", noah),
    {noreply, State};
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
extract_info_from_prado_page(Body) ->
    Tree = mochiweb_html:parse(Body),
    Title = extract_text(mochiweb_xpath:execute("//span[@id='ctl0_main_ctlLLName']/text()", Tree)),
    %% 上线单状态： 上线完成
    Status = extract_text(mochiweb_xpath:execute("//span[@id='ctl0_main_ctlLLStatus']/text()", Tree)),
    %% 正常结束：165 异常结束：0 暂停：0 未开始：0 进行中：0
    MachineStatus = lists:map(fun(I) -> list_to_integer(binary_to_list(I)) end,
                              mochiweb_xpath:execute("//div[@id='orderStatus']//span/text()", Tree)),
    [Concurrency| _] = string:tokens(lists:nth(2, string:tokens(
                                                    unicode:characters_to_list(
                                                      lists:last(mochiweb_xpath:execute("//div[@class='container']/div[1]/text()", Tree)), utf8),
                                                    [10, 13, 32, 40, 41])),
                                     "_"),
    %% io:format("title: ~ts~n", [Title]),
    %% io:format("status: ~ts~n", [Status]),
    %% io:format("machine: ~p~n", [MachineStatus]),
    %% io:format("concurrency: ~ts~n", [Concurrency]),
    GroupInfo = lists:map(fun(G) ->
                                  [GName, ConcurrencyStr, GStatus | _] = string:tokens(
                                                                           unicode:characters_to_list(
                                                                             lists:last(mochiweb_xpath:execute("div/div[1]/text()", G)), utf8),
                                                                           [10, 13, 32, 40, 41]),
                                  [Concurrency, GConcurrency] = string:tokens(ConcurrencyStr, "_"),
                                  %% io:format("gname: ~ts~n", [GName]),
                                  %% io:format("gstatus: ~ts~n", [GStatus]),
                                  %% io:format("gconcurrency: ~ts~n", [GConcurrency]),
                                  Machines = lists:map(fun(M) ->
                                                               MName = extract_text(mochiweb_xpath:execute("tr/td[1]/text()", M)),
                                                               MStatus = extract_text(mochiweb_xpath:execute("tr/td[2]/text()", M)),
                                                               MRevertStatus = extract_text(mochiweb_xpath:execute("tr/td[3]/text()", M)),
                                                               {machine, MName, MStatus, MRevertStatus}
                                                       end,
                                                       mochiweb_xpath:execute("div//table[2]/tbody[2]/tr", G)),
                                  {group, GName, GStatus, GConcurrency, Machines}
                          end,
                          mochiweb_xpath:execute("//div[@class='container']", Tree)),
    {deploy_info, Title, Status, MachineStatus, Concurrency, GroupInfo}.

extract_info_from_archer_page(Body) ->
    Tree = mochiweb_html:parse(Body),
    Title = extract_text(mochiweb_xpath:execute("//h3[2]/span[1]/text()", Tree)),
    %% 上线单状态： 上线完成
    %% 使用全角空格分割
    [StatusBin] = mochiweb_xpath:execute("//h3[1]/span[1]/text()", Tree),
    [_, Status] = string:tokens(unicode:characters_to_list(StatusBin, utf8), [160]),
    %% 成功的机器：200 失败的机器：0 运行中的机器： 0 未执行
    MachineStatus = lists:map(fun(I) -> list_to_integer(binary_to_list(I)) end,
                              mochiweb_xpath:execute("//div[@id='orderStatus']//span/text()", Tree)),
    %% 页面比较变态, 为并行的时候会用个红色的 span 包着
    Concurrency = lists:last(["unkown"] ++ lists:map(
                                             fun(Bin) when is_binary(Bin) ->
                                                     lists:last(string:tokens(unicode:characters_to_list(Bin, utf8), [10, 13, 32]))
                                             end,
                                             mochiweb_xpath:execute("//div[@id='V-main']/div/div[4]//text()", Tree))),
    %% io:format("title: ~ts~n", [Title]),
    %% io:format("status: ~ts~n", [Status]),
    %% io:format("machine: ~p~n", [MachineStatus]),
    %% io:format("concurrency: ~ts~n", [Concurrency]),
    GroupInfo = lists:map(fun(G) ->
                                  [{_, _, ChildrenNodes}] = mochiweb_xpath:execute("div[1]/div[1]", G),
                                  %% strip [, ]
                                  [GName] = string:tokens(extract_text(lists:nth(1, ChildrenNodes)), "[]"),
                                  GStatus = extract_text(lists:nth(5, ChildrenNodes)),
                                  %% 页面比较变态, 为并行的时候会用个红色的 span 包着
                                  %% 并发度不为0/1时候不显示...我操
                                  GConcurrency = lists:last(
                                                   ["unkown"] ++ string:tokens(
                                                                   unicode:characters_to_list(
                                                                     lists:last(mochiweb_xpath:execute("div[1]/div[1]/div[1]//text()", G)), utf8),
                                                                   [10, 13, 32])),
                                  %% io:format("gname: ~ts~n", [GName]),
                                  %% io:format("gstatus: ~ts~n", [GStatus]),
                                  %% io:format("gconcurrency: ~ts~n", [GConcurrency]),
                                  Machines = lists:map(fun(M) ->
                                                               MName = extract_text(mochiweb_xpath:execute("tr/td[1]/text()", M)),
                                                               MStatus = extract_text(mochiweb_xpath:execute("tr/td[2]/text()", M)),
                                                               MRevertStatus = undefined,
                                                               {machine, MName, MStatus}
                                                       end,
                                                       mochiweb_xpath:execute("div//table[2]/tbody[2]/tr", G)),
                                  {group, GName, GStatus, GConcurrency, Machines}
                          end,
                          mochiweb_xpath:execute("//div[@class='container csu']", Tree)),
    {deploy_info, Title, Status, MachineStatus, Concurrency, GroupInfo}.

extract_text([Binary]) when is_binary(Binary) ->
    extract_text(Binary);
extract_text(Binary) when is_binary(Binary) ->
    %% convert to unicode
    lists:flatten(string:tokens(unicode:characters_to_list(Binary, utf8), [10, 13, 32])).

http_request(Method, Url, Params) ->
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
                                               [], noah);
        post ->
            {ok, {_, _, Body}} = httpc:request(post, {Url, [], "application/x-www-form-urlencoded", util:url_encode(Params)},
                                               [],
                                               [], noah)
    end,
    {ok, Body}.
