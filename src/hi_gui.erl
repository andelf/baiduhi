%% This module is based on ex_radioBox.erl located in
%% -*- encoding: utf-8; -*-
%%   lib/wx/examples/demo
%% Differences from ex_radioBox.erl are marked with %%CHANGES
%% See online discussion of the major changes

-module(hi_gui).
-author("Doug Edmunds").
-behaviour(wx_object).

%%CHANGES -- added start/0, removed start/1

-export([start/0, init/1, terminate/2,  code_change/3, handle_cast/2,
	 handle_info/2, handle_call/3, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state,
	{
	  parent,
	  config,
	  chat_win,
	  input_ctl,
          contact_list_ctl
	}).

%%CHANGES -- new function start/0

start() ->
    Server = wx:new(),
    io:format("Server: ~p~n",[Server]),

    Title = lists:flatten(io_lib:format("百度Hi - ~ts(~p)", [baiduhi:baiduid(), baiduhi:imid()])),
    Frame = wxFrame:new(Server, -1, Title, [{size,{640, 480}}]),
    io:format("Frame: ~p~n",[Frame]),

    Config = [{parent, Frame}],
    wx_object:start_link(?MODULE, Config, []).


% CHANGES -- removed function start/1
% start(Config) ->
%    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    hi_event:add_handler(hi_event_forwarder, self()),
    wx:batch(fun() -> do_init(Config) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{event = #wxCommand{type = command_radiobox_selected,
				    cmdString = Item}},
	     State = #state{}) ->
    format(State#state.config,"wxRadioBox selected ~p\n",[Item]),
    {noreply, State};

handle_event(#wx{obj  = Checkbox,
		 event = #wxCommand{type = command_checkbox_clicked,
				    commandInt = Int}},
	     State = #state{config = Config}) ->
    Label = wxCheckBox:getLabel(Checkbox),
    case Int of
	0 -> format(Config,"wxCheckBox deselected ~p\n",[Label]);
	1 -> format(Config,"wxCheckBox selected ~p \n",[Label]);
	2 -> format(Config,"wxCheckBox middle-state ~p\n",[Label])
    end,
    {noreply, State};

handle_event(#wx{obj  = Button,
		 event = #wxCommand{type = command_button_clicked}},
	     State = #state{chat_win=ChatWin,input_ctl=InputCtl, contact_list_ctl=ContactCtl}) ->
    Props = get(contact_lists),
    Label = wxButton:getLabel(Button),
    Text = wxTextCtrl:getValue(InputCtl),
    wxTextCtrl:setValue(InputCtl, ""),
    {1, [Selected]} = wxListBox:getSelections(ContactCtl),
    Contact = lists:nth(Selected, Props),
    Who = proplists:get_value(nickname, Contact),
    wxHtmlWindow:appendToPage(ChatWin,
			      io_lib:format("你对~ts说: ~ts<br />", [Who, Text])),
    baiduhi:send_single_message(proplists:get_value(imid, Contact),
                                Text),
    format(State#state.config,"wxButton clicked ~p: ~p\n",[Label, Contact]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info({contact_notify, Imid, Prop}, State = #state{chat_win=ChatWin}) ->
    {ok, Who} = baiduhi:imid_to_baiduid(Imid),
    wxHtmlWindow:appendToPage(ChatWin,
			      io_lib:format("~ts 状态: ~p<br />", [Who, Prop])),
    {noreply, State};
handle_info({text_msg_notify,Text,From,Type,ReplyTo}, State = #state{chat_win=ChatWin}) ->
    {ok, Who} = baiduhi:imid_to_baiduid(From),
    wxHtmlWindow:appendToPage(ChatWin,
			      io_lib:format("~ts 说: ~ts<br />", [Who, Text])),
    {noreply, State};
handle_info(Msg, State) ->
    format(State#state.config, "Got Info ~p\n",[Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    format(State#state.config,"Got Call ~p\n",[Msg]),
    {reply, {error, nyi}, State}.

code_change(_, _, State) ->  %%this is just a stub
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    wx:destroy(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_init(Config) ->
    Parent = proplists:get_value(parent, Config),
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    RightSizer = wxBoxSizer:new(?wxVERTICAL),

    {ok, Friends} = baiduhi:get_friends(),
    Choices = begin
                  {ok, Props} = baiduhi:query_contacts(lists:map(fun(Prop) ->
                                                                         proplists:get_value(imid, Prop)
                                                                 end, Friends)),
                  put(contact_lists, Props),
                  lists:map(fun(Prop) ->
                                    Imid = proplists:get_value(imid, Prop),
                                    Id = proplists:get_value(baiduid, Prop),
                                    Nick = proplists:get_value(nickname, Prop),
                                    erlang:put(Imid, {Id, Nick}),
                                    Nick ++ "(" ++ Id ++ ")"
                            end, Props)
              end,
    ListBoxSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
					[{label, "我的基友"}]),
    %% fake 0th item
    ListBox = wxListBox:new(Panel, 1, [{size, {100,400}},
				       {choices, ["请选择你的基友"|Choices]}]),
				       %%{style, ?wxLB_MULTIPLE}]),
    wxListBox:setToolTip(ListBox, "我的基友列表"),
    %%wxRadioBox:connect(ListBox, command_radiobox_selected),
    wxListBox:connect(ListBox, command_listbox_doubleclicked),
    Options = [{border,4}, {flag, ?wxALL}],

    wxSizer:add(ListBoxSizer, ListBox, Options),

    %% RadioButtonSizer = create_radio_buttons(Panel),
    {ChatWinSizer, ChatWin} = create_chatwindow(Panel),
    {TextSizer, TextCtrl} = create_textbox(Panel),

    %% Add to sizers
    wxSizer:add(Sizer, ListBoxSizer),

    %wxSizer:add(RightSizer, RadioButtonSizer),
    wxSizer:add(RightSizer, ChatWinSizer),
    wxSizer:addSpacer(RightSizer, 20),
    wxSizer:add(RightSizer, TextSizer),

    wxSizer:add(Sizer, wxSplitterWindow:new(Panel, []), [{flag, ?wxEXPAND},
                                                         {proportion, 1}]),

    wxSizer:add(Sizer, RightSizer),

    wxSizer:addSpacer(MainSizer, 20),
    wxSizer:add(MainSizer, Sizer),

    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:show(Parent),

    {Panel, #state{parent=Panel, config=Config, contact_list_ctl=ListBox,
		   chat_win=ChatWin, input_ctl=TextCtrl}}.

create_textbox(Panel) ->
    TextSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,
				     [{label, "聊天内容"}]),
    TextCtrl = wxTextCtrl:new(Panel, 3, [{size, {300,50}},
					 {value, "这里输入你的聊天内容"},
					 {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    B10 = wxButton:new(Panel, 10, [{label,"发送"}, {size, {-1, 50}}]),
    wxButton:connect(B10, command_button_clicked),
    wxSizer:add(TextSizer, TextCtrl, [{flag, ?wxALL},{proportion, 1}]),
    wxSizer:add(TextSizer, B10),
    {TextSizer, TextCtrl}.


create_chatwindow(Panel) ->
    ChatWinSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
					[{label, "聊天窗口"}]),
    HtmlWin = wxHtmlWindow:new(Panel, [{size, {350, 300}}]),
    wxSizer:add(ChatWinSizer, HtmlWin, [{flag, ?wxEXPAND}, {proportion, 1}]),
    {ChatWinSizer, HtmlWin}.

%% CHANGES - format/2 added.
format(_Config, Str, Args) ->
    io:format(Str,Args),
    ok.
