# Baidu Hi erlang version

这个是 Baidu Hi 的客户端，Erlang 写的，使用客户端协议。

This is a Baidu Hi client, written by erlang.

请区别 Baidu Id, Imid.

使用方法:

Usage:

    $ rebar get-deps
    $ rebar compile
    $ ERL_LIBS=./deps erl -pa ../baiduhi/ebin -s lager
    ...
    > baiduhi:start().

用法2:

Alternative:

    $ make
    $ make run
    $ make clean

注意:

Note:

    DO TO fill `priv/baiduhi.conf` first.

    All API in baiduhi module.
