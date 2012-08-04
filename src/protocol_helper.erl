%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(protocol_helper).

%% API
%%-export([]).
-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================


'login#login'({V_Url, V_Time, V_Period, V_Code}, DynamicPassword) ->
    VerifyHeaders = [{v_url, V_Url},
                      {v_time, V_Time},
                      {v_period, V_Period},
                      {v_code, V_Code}],
    'login#login'(VerifyHeaders, DynamicPassword);
'login#login'(VerifyHeaders, DynamicPassword) ->
    Params = [{method, "login"}|VerifyHeaders],
    Timestamp = util:to_list(util:timestamp()),
    Body = util:tuple_to_xml(
             {login, [], [{user, [{account, hi_state:get(username)},
                                  {password, DynamicPassword},
                                  {localtime, Timestamp},
                                  {imversion, "2,1,0,1"}], []}]}),
    {{login, "3.0", request, hi_state:seq()}, Params, Body}.

'login#loginout'() ->
    {{login, "1.0", request, hi_state:seq()},
     [{method, "loginout"}],
     ""}.

%% VerifyCodeUnknown       = 0,
%% VerifyCodeLogin			= 1,//登录验证码, uid=0
%% VerifyCodeAddFriend		= 2,//添加好友
%% VerifyCodeDeleteFriend	= 3,//删除好友
%% VerifyCodeTransferGroup = 4,//转让群
%% VerifyCodeCreateGroup	= 5,//创建群
%% VerifyCodeSendBaiduMsg  = 6,//发送站内消息
%% VerifyCodeDisbandGroup	= 7,//解散群,服务器内部使用,客户端不能请求.
%% VerifyCodeJoinGroup	= 8,//加入群
%% VerifyCodeQuitGroup	= 9,//退出群
%% VerifyCodeSendEmail	= 10,//发送 email
%% VerifyCodeTmpSession = 11//发送临时会话消息,需要带uid2:
'security#verify'(login) ->
    {{security, "1.0", request, hi_state:seq()},
     [{method, "verify"}, {uid, 0}, {type, 1}, {lid, hi_state:get(username)}],
     ""};
'security#verify'(delete_friend) ->
    'security#verify'(3, []);
'security#verify'(join_group) ->
    'security#verify'(8, []);
'security#verify'(quit_group) ->
    'security#verify'(9, []).
'security#verify'(temp_chat, Imid) ->
    'security#verify'(11, [{uid2, Imid}]);
'security#verify'(add_friend, Imid) ->
    'security#verify'(2, [{friend, Imid}]);
'security#verify'(delete_friend, Imid) ->
    'security#verify'(3, [{friend, Imid}]);
'security#verify'(Type, AdditonalHeader) ->
    {{security, "1.0", request, hi_state:seq()},
     [{method, "verify"}, {uid, hi_state:uid()}, {type, Type},
      {lid, hi_state:get(username)}|AdditonalHeader],
     ""}.

%% friend
'friend#get_friend'() ->
    {{friend, "1.0", request, hi_state:seq()},
     [{method, "get_friend"},
      {uid, hi_state:uid()},
      {page, 0},
      {timestamp, 0}],
     []}.

'friend#get_team'() ->
    {{friend, "1.0", request, hi_state:seq()},
     [{method, "get_team"},
      {uid, hi_state:uid()}],
     []}.

'friend#find'(Account) ->
    {{friend, "1.0", request, hi_state:seq()},
     [{method, "find"},
      {uid, hi_state:uid()},
      {account, Account},
      {type, case lists:member($@, Account) of
                 true -> 2;                     % email
                 _Other -> 1                    % baiduid
             end}],
     []}.

'friend#q_type'(Imid) ->
    {{friend, "1.0", request, hi_state:seq()},
     [{method, "q_type"},
      {uid, hi_state:uid()},
      {q, Imid}],
     []}.

'friend#add_ack'(Agree, Imid) ->
    'friend#add_ack'(Agree, Imid, "").
'friend#add_ack'(Agree, Imid, RejectReason) ->
    {{friend, "1.0", request, hi_state:seq()},
     [{method, "add_ack"},
      {uid, hi_state:uid()}],
     util:tuple_to_xml(
       {add_ack, [{time, util:to_list(util:timestamp())},
                  {agree, util:to_list(Agree)},
                  {imid, util:to_list(Imid)},
                  {reject_reason, RejectReason}],
        []})}.

'friend#add'(VerifyHeaders, Imid, RequestNote) ->
    {{friend, "1.0", request, hi_state:seq()},
     [{method, "add"},
      {uid, hi_state:uid()}|VerifyHeaders],
     util:tuple_to_xml(
       {add_friend, [{time, util:to_list(util:timestamp())},
                     {imid, util:to_list(Imid)},
                     {team, "0"},
                     {request_note, util:to_list(RequestNote)}],
        []})}.

'friend#delete'(VerifyHeaders, Imid) ->
    {{friend, "1.0", request, hi_state:seq()},
     [{method, "delete"},
      {uid, hi_state:uid()}|VerifyHeaders],
     util:tuple_to_xml(
       {delete_friend, [{imid, util:to_list(Imid)}], []})}.

%% user
%% user:set
%% nickname="xxx"
%% personal_comment="xxx"
%% head="md5;name;type"
%% friendly_level="1|2|3"
%% info_open_level="xxx"
%% name="xxx"
%% sex="xxx"
%% birthday="xxxx-xx-xx"
%% email="xxxx"
%% personal_desc="xxx"
%% frequency_sort="1|0"
%% has_camera="1|0"
%% status="type[;remark]"
%% music="xxx"
%% phone="xxx"
%% visible="0|1"
%% sns_modsign_visible="0/1"
%% sns_modheadpic_visible="0/1"
%% sns_levelup_visible="0/1"
%% sns_postscript_visible="0/1"
%% sns_space_visible="0/1"
%% sns_tieba_visible="0/1"
%% sns_iknow_visible="0/1"
'user#set'(Infos) ->
    {{user, "2.5", request, hi_state:seq()},
     [{method, "set"},
      {uid, hi_state:uid()}],
     util:tuple_to_xml(
       {user, [],
        [{account, Infos, []}]
       })}.

%% baiduid;
%% birthday;
%% email[;email_fixed 是否验证];
%% frequency_sort;
%% friendly_level;
%% head;
%% info_open_level;
%% key;
%% name;
%% nickname;
%% personal_comment;
%% personal_desc;
%% phone;
%% psp_msg_count;
%% sex;
%% tmsg_policy;
%% visible;
%% vitality; "uint32_t(当前活跃天数);uint32_t(当前等级/生命力);uint32_t(升级下一级还需要的天数)"
%% wealth;
%% sns_visible
'user#query'(Fields) ->
    {{user, "1.10", request, hi_state:seq()},
     [{method, "query"},
      {uid, hi_state:uid()}],
     util:tuple_to_xml(
       {'query', [{fields, string:join(Fields, ";")}], []})}.

'user#login_ready'() ->
    'user#login_ready'("1").
'user#login_ready'(Status) ->
    {{user, "1.0", request, hi_state:seq()},
     [{method, "login_ready"},
      {uid, hi_state:uid()}],
     util:tuple_to_xml(
       {login, [], [{user, [{status, util:to_list(Status)},
                            {localeid, "2052"},
                            {imversion, "2,1,0,1"},
                            {pc_code, "BDIMXV2-O_1-C_9-D_3-M_4-V_5"}], []}]})}.
        %% ,LocaleSimpleChinese = 2052 /**< 简体中文.*/
        %% ,LocaleTraditionalChinese= 1028 /**< 繁体中文.*/
        %% ,LocaleUsEnglish = 1033 /**< 美国英语.*/
        %% ,LocaleJapanese = 1041 /**< 日文 >*/


%% contact
%% contact:query
%% Fields imid;baiduid; status; personal_comment ; nickname ; head ;
%% camera; friendly_level ; name ; homepage ; birthday ; personal_desc ;
%% info_open_level ; email ; email_fixed ; music ; cli_type ; cli_ver;
%% psp_status; vitality;
%% vitality="uint32_t(活跃天数);uint32_t(等级);uint32_t(下一级需要天数)"
'contact#query'(Fields, ImidList) ->
    {{contact, "3.15", request, hi_state:seq()},
     [{method, "query"},
      {uid, hi_state:uid()}],
     %% query is a reversed word
     util:tuple_to_xml(
       {'query', [{fields, string:join(Fields, ";")},
                  {id, string:join(ImidList, ";")}],
             []})}.

%% 从大到小返回
'contact#queryonline'() ->
    'contact#queryonline'(1).
'contact#queryonline'(LastId) ->
    {{contact, "1.0", request, hi_state:seq()},
     [{method, "queryonline"},
      {uid, hi_state:uid()},
      {lastid, LastId},                       % 最小的uid
      {count, 30}],                           % 返回的最大数目, 最大30
     []}.

%% group
'group#get_list'() ->
    {{group, "1.0", request, hi_state:seq()},
     [{method, "get_list"},
      {timestamp, util:timestamp()},
      {uid, hi_state:uid()}],
     ""}.

'group#get'(Gid) ->
    {{group, "1.0", request, hi_state:seq()},
     [{method, "get"},
      {uid, hi_state:uid()},
      {gid, Gid}],
     ""}.

'group#get_member'(Gid) ->
    {{group, "1.5", request, hi_state:seq()},
     [{method, "get_member"},
      {uid, hi_state:uid()},
      {gid, Gid},
      {timestamp, 0},
      {pagesize, 1000},
      {page, 0}],
     ""}.

%% name="string" phone="string" email="string" remark="string"
'group#set_card'(Gid, InfoAssoc) ->
    {{group, "1.0", request, hi_state:seq()},
     [{method, "set_card"},
      {uid, hi_state:uid()},
      {gid, Gid}],
     util:tuple_to_xml(
       {card, InfoAssoc, []})}.

'group#set_card2'(Gid, Member, InfoAssoc) ->
    {{group, "1.0", request, hi_state:seq()},
     [{method, "set_card2"},
      {uid, hi_state:uid()},
      {gid, Gid},
      {member, Member}],
     util:tuple_to_xml(
       {card, InfoAssoc, []})}.


'group#add_member'(Gid, ImidList) ->
    {{group, "1.0", request, hi_state:seq()},
     [{method, "add_member"},
      {uid, hi_state:uid()},
      {gid, Gid}],
     util:tuple_to_xml(
       {add_member, [], lists:map(fun(Imid) ->
                                          {member, [{imid, Imid}], []}
                                  end,
                                  ImidList)})}.

%% 管理员不可以删除管理员，群主可以删除自己之外的任何人
'group#delete_member'(Gid, ImidList) ->
    {{group, "1.0", request, hi_state:seq()},
     [{method, "delete_member"},
      {uid, hi_state:uid()},
      {gid, Gid}],
     util:tuple_to_xml(
       {delete_member, [], lists:map(fun(Imid) ->
                                             {member, [{imid, Imid}], []}
                                  end,
                                  ImidList)})}.

%% 群成员使用本方法可以从群中退出
'group#quit'(Gid) ->
    {{group, "1.0", request, hi_state:seq()},
     [{method, "quit"},
      {uid, hi_state:uid()},
      {gid, Gid}],
     []}.

'group#join'(Gid) ->
    'group#join'(Gid, "").
'group#join'(Gid, RequestNote) ->
    {{group, "1.0", request, hi_state:seq()},
     [{method, "join"},
      {uid, hi_state:uid()},
      {gid, Gid}],
     util:tuple_to_xml(
       {join_group, [{time, util:to_list(util:timestamp())},
                     {request_note, RequestNote}],
        []})}.


%% msg
'msg#msg_request'(Type, To, Message) ->
    {{msg, "1.1", request, hi_state:seq()},
     [{method, "msg_request"},
      {type, Type}, {uid, hi_state:uid()}, {from, hi_state:uid()},
      {to, To}, {time, util:timestamp()},
      {waitack, 120},
      {basemsgid, hi_state:seq(no_increase)},
      {msgid, 0}, {subid, 0}, {nextsubid, 0}],
     Message}.

'msg#msg_ack'(Type, To, MsgId) ->
    {{msg, "1.0", request, hi_state:seq()},
     [{method, "msg_ack"},
      {type, Type}, {uid, hi_state:uid()}, {from, hi_state:uid()}, {to, To},
      {ack, MsgId}],
     ""}.

'msg#tmsg_request'({V_Url, V_Time, V_Period, V_Code}, To, Message) ->
    VerifyHeaders = [{v_url, V_Url},
                      {v_time, V_Time},
                      {v_period, V_Period},
                      {v_code, V_Code}],
    'msg#tmsg_request'(VerifyHeaders, To, Message);
'msg#tmsg_request'(VerifyHeaders, To, Message) ->
    {{msg, "1.1", request, hi_state:seq()},
     [{method, "tmsg_request"},
      {uid, hi_state:uid()},
      {from, hi_state:uid()},
      {type, 4},
      {to, To},
      {time, util:timestamp()},
      {basemsgid, hi_state:seq(no_increase)},
      {msgid, 0}, {subid, 0}, {nextsubid, 0}|VerifyHeaders],
     Message}.

%% cm 通用消息相关命令
'cm#blk'(To) ->
    {{cm, "1.0", request, hi_state:seq()},
     [{method, "blk"}, {type, 1},
      {from, hi_state:uid()}, {uid, hi_state:uid()},
      {to, To}],
     ""}.

'cm#typ'(To) ->
    Type = 1,                                   % only type=1
    {{cm, "1.0", request, hi_state:seq()},
     [{method,"typ"},
      {uid, hi_state:uid()},
      {type, Type},
      {to, To},
      {sub_id,0},
      {from, hi_state:uid()}],
     []}.

'cm#ntyp'(To) ->
    Type = 1,                                   % only type=1
    {{cm, "1.0", request, hi_state:seq()},
     [{method,"ntyp"},
      {uid, hi_state:uid()},
      {type, Type},
      {to, To},
      {sub_id,0},
      {from, hi_state:uid()}],
     []}.

%% multi

%% join_enable="1or0" add_enable="1or0" find_enable="1or0" hide_enable="1or0"
'multi#create'(ImidList) ->
    {{multi, "1.0", request, hi_state:seq()},
     [{method, "create"},
      {uid, hi_state:uid()},
      {vendor_name, "我了个去"}],               % useless
     util:tuple_to_xml(
       {member_set, [{join_enable, "1"},
                     {add_enable, "1"},
                     {find_enable, "1"},
                     {hide_enable, "0"}],
        lists:map(fun(Imid) ->
                          {member, [{imid, Imid}], []}
                  end, ImidList)})}.

'multi#quit'(Mid) ->
    {{multi, "1.0", request, hi_state:seq()},
     [{method, "quit"},
      {uid, hi_state:uid()},
      {mid, Mid}],
     ""}.

'multi#get_list'(Mid) ->
    {{multi, "1.0", request, hi_state:seq()},
     [{method, "get_list"},
      {uid, hi_state:uid()},
      {mid, Mid}],
     ""}.

'multi#add'(Mid, ImidList) ->
    {{multi, "1.0", request, hi_state:seq()},
     [{method, "add"},
      {uid, hi_state:uid()},
      {mid, Mid}],
     util:tuple_to_xml(
       {member_set, [],
        lists:map(fun(Imid) ->
                          {member, [{imid, Imid}], []}
                  end, ImidList)})}.

%% 申请图片表情服务器的地址（TCP）
imagesvr() ->
    {{imagesvr, "1.0", request, hi_state:seq()}, [{from, hi_state:uid()}], []}.

%% [2,210,254,3,123,125,113,34,1,187]
parse_imagesvr(Data) when is_list(Data) ->
    parse_imagesvr(list_to_binary(Data));
parse_imagesvr(Data) when is_binary(Data) ->
    <<Token:16/little-unsigned-integer,
      IP:32/little-unsigned-integer,
      Port:16/little-unsigned-integer, _/binary>> = Data,
    {Token, IP, Port}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
