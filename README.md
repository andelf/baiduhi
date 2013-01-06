Baidu Hi erlang version
=======================

这个是 Baidu Hi 的客户端，Erlang 写的，使用客户端协议。

This is a Baidu Hi client, written by erlang.

使用方法:

Usage:

    $ rebar get-deps
    $ rebar compile
    $ erl -pa ../baiduhi/ebin -pa deps/mochiweb/ebin -pa deps/mochixpath/ebin
    ...
    > baiduhi:start().

注意:

Note:

    DO TO fill `priv/baiduhi.conf` first.

    All API in baiduhi module.
