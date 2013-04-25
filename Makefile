.PHONY: run
.PHONY: all
.PHONY: clean


# <del>for lager to handle chinese, we must use UNICODE_AS_BINARIES</del>
# for some reason my erlang shell doesn't support unicode
# so do to make sure:
#
all:
	./rebar get-deps
	./rebar compile

run:
	ERL_LIBS=deps erl -pa ../baiduhi/ebin -s lager

clean:
	./rebar clean
