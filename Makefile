APP := dog

.PHONY: all
all:
	@(./rebar3 compile)

.PHONY: shell
shell:
	@(ERL_FLAGS="-config config/sys.config" ./rebar3 shell)

.PHONY: test
test:
	@(./rebar3 eunit)
