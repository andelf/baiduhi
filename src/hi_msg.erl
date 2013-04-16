%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%% hi msg format
%%% @end
%%% Created : 14 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(hi_msg).

%% API
-export([img_tag/1, make_imgdata/1, make_msg/1, make_msg/2]).

%%%===================================================================
%%% API
%%%===================================================================
make_imgdata(Data) ->
    Md5 = util:to_hex_string(crypto:md5(Data)),
    ImageData = base64:encode_to_string(Data),
    {ok, Md5, ImageData}.

img_tag({imgdata, Type, Data}) ->
    {ok, Md5, ImageData} = make_imgdata(Data),
    {img, [{md5, Md5}, {t, Type}, {n, lists:sublist(Md5, 8)}],
     [{image, [{imagedata, ImageData}], []}]}.

make_msg(Text) ->
    make_msg(Text, []).
make_msg(Text, Prop) ->
    util:tuple_to_xml({msg, [], [prop_to_fontspec(Prop),
                                 {text, [{c, util:to_list(Text)}], []}
                                ]}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
prop_to_fontspec(Prop) ->
    Font = proplists:get_value(font, Prop, "Fixedsys"),
    FontSize = proplists:get_value(size, Prop, 10),
    Bolded = proplists:get_bool(bold, Prop),
    Italic = proplists:get_bool(italic, Prop),
    Underlined = proplists:get_bool(underline, Prop),
    Color = proplists:get_value(color, Prop, 16#EE9640),
    {font, [{n, Font}, {s, FontSize}, {b, Bolded}, {i, Italic},
            {ul, Underlined}, {c, Color}, {cs, 134}],
     []}.
