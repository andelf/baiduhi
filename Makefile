.PHONY: run
.PHONY: all
.PHONY: clean


all:
	./rebar get-deps
	./rebar compile

run:
	ERL_LIBS=deps erl -pa ../baiduhi/ebin -s lager

clean:
	./rebar clean
