.PHONY: run
.PHONY: all
.PHONY: clean


# for lager to handle chinese, we must use UNICODE_AS_BINARIES
all:
	./rebar get-deps
	./rebar compile -D UNICODE_AS_BINARIES

run:
	ERL_LIBS=deps erl -pa ../baiduhi/ebin -s lager

clean:
	./rebar clean
