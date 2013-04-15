%%%-------------------------------------------------------------------
%%% @author Feather.et.ELF <fledna@qq.com>
%%% @copyright (C) 2012, Feather.et.ELF
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2012 by Feather.et.ELF <fledna@qq.com>
%%%-------------------------------------------------------------------
-module(util).

%% API
-export([timestamp/0, timestamp/1, pkcs1_to_rsa_pubkey/1, pkcs1_to_rsa_prvkey/1]).
-export([rsa_public_decrypt/2, rsa_public_encrypt/2, rsa_private_decrypt/2,
        aes_encrypt/2, aes_decrypt/2]).
-export([tuple_to_xml/1, xml_to_tuple/1]).
-export([escape_uri/1, url_encode/1]).
-export([to_hex_string/1, to_list/1, to_integer/1]).



%%%===================================================================
%%% API
%%%===================================================================

timestamp() ->
    timestamp(now()).
timestamp({Mega, Secs, Micro}) ->
    (Mega * 1000 * 1000 + Secs) * 1000 + (Micro div 1000).

%% pkcs1 to rsa key
pkcs1_to_rsa_pubkey(PubPkcs1) ->
    %% asn1ct:compile('PKCS-1'),
    {ok, {'RSAPublicKey', N, E}} = asn1ct:decode('PKCS-1', 'RSAPublicKey', PubPkcs1),
    [crypto:mpint(E), crypto:mpint(N)].

pkcs1_to_rsa_prvkey(PrvPkcs1) ->
    %% asn1ct:compile('PKCS-1'),
    {ok, {'RSAPrivateKey', _, N, E, D, _, _, _, _, _, _}} = asn1ct:decode('PKCS-1', 'RSAPrivateKey', PrvPkcs1),
    [crypto:mpint(E), crypto:mpint(N), crypto:mpint(D)].

tuple_to_xml(Data) ->
    Doc = xmerl:export_simple_content([Data], xmerl_xml),
    binary:list_to_bin(Doc).

xml_to_tuple(Xml) ->
    {Doc, _} = xmerl_scan:string(Xml),
    xmerl:export([Doc], xmerl_tuple).

%% crypto
rsa_public_decrypt(Data, Key) ->
    rsa_public_decrypt(Data, Key, <<>>).
rsa_public_decrypt(<<Chipher:128/binary, Tail/binary>>, Key, Acc) ->
    Plain = crypto:rsa_public_decrypt(Chipher, Key, rsa_pkcs1_padding),
    rsa_public_decrypt(Tail, Key, <<Acc/binary, Plain/binary>>);
rsa_public_decrypt(<<>>, _, Acc) -> Acc.

rsa_public_encrypt(Data, Key) ->
    rsa_public_encrypt(Data, Key, <<>>).
rsa_public_encrypt(<<Plain:100/binary, Tail/binary>>, Key, Acc) ->
    Chipher = crypto:rsa_public_encrypt(Plain, Key, rsa_pkcs1_padding),
    rsa_public_encrypt(Tail, Key, <<Acc/binary, Chipher/binary>>);
rsa_public_encrypt(Data, Key, Acc) when byte_size(Data) > 0 ->
    Padding = binary:list_to_bin(lists:duplicate(100-byte_size(Data), 0)),
    rsa_public_encrypt(<<Data/binary, Padding/binary>>, Key, Acc);
rsa_public_encrypt(<<>>, _, Acc) -> Acc.

rsa_private_decrypt(Data, Key) ->
    rsa_private_decrypt(Data, Key, <<>>).
rsa_private_decrypt(<<Chipher:128/binary, Tail/binary>>, Key, Acc) ->
    Plain = crypto:rsa_private_decrypt(Chipher, Key, rsa_pkcs1_padding),
    rsa_private_decrypt(Tail, Key, <<Acc/binary, Plain/binary>>);
rsa_private_decrypt(<<>>, _, Acc) -> Acc.

aes_encrypt(Data, Key) ->
    aes_encrypt(Data, Key, <<>>).
aes_encrypt(<<Plain:16/binary, Tail/binary>>, Key, Acc) ->
    Chipher = crypto:aes_cbc_128_encrypt(Key, <<0:128>>, Plain),
    aes_encrypt(Tail, Key, <<Acc/binary, Chipher/binary>>);
aes_encrypt(Data, Key, Acc) when byte_size(Data) > 0 ->
    Padding = binary:list_to_bin(lists:duplicate(16-byte_size(Data), 0)),
    aes_encrypt(<<Data/binary, Padding/binary>>, Key, Acc);
aes_encrypt(<<>>, _, Acc) -> Acc.

aes_decrypt(Data, Key) ->
    aes_decrypt(Data, Key, <<>>).
aes_decrypt(<<Chipher:16/binary, Tail/binary>>, Key, Acc) ->
    Plain = crypto:aes_cbc_128_decrypt(Key, <<0:128>>, Chipher),
    aes_decrypt(Tail, Key, <<Acc/binary, Plain/binary>>);
aes_decrypt(<<>>, _, Acc) -> Acc.


url_encode(Data) ->
    url_encode(Data,"").
url_encode([],Acc) ->
    Acc;
url_encode([{Key,Value}|R],"") ->
    url_encode(R, escape_uri(Key) ++ "=" ++ escape_uri(Value));
url_encode([{Key,Value}|R],Acc) ->
    url_encode(R, Acc ++ "&" ++ escape_uri(Key) ++ "=" ++ escape_uri(Value)).


escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(S) when is_atom(S) ->
    escape_uri(atom_to_list(S));
escape_uri(S) when is_integer(S) ->
    escape_uri(integer_to_list(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].

to_hex_string(Data) when is_list(Data) ->
    to_hex_string(binary:list_to_bin(Data));
to_hex_string(Data) when is_binary(Data)->
    OctMap = "0123456789abcdef",
    lists:flatten([lists:nth(1+N, OctMap) || <<N:4>> <= Data]).


to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(<<>>) ->
    0;
to_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_list(List) when is_list(List) ->
    %% FIXME: handle ordernary list
    case lists:all(fun(C) -> C < 256 end, List) of
        true ->
            List;
        false ->
            binary_to_list(unicode:characters_to_binary(List))
    end;
to_list(Integer) when is_integer(Integer) ->
    integer_to_list(Integer).

to_integer(List) when is_list(List) ->
    list_to_integer(List);
to_integer(Integer) when is_integer(Integer) ->
    Integer;
to_integer(<<>>) ->
    0;
to_integer(Binary) when is_binary(Binary) ->
    list_to_integer(binary_to_list(Binary)).




%%%===================================================================
%%% Internal functions
%%%===================================================================
