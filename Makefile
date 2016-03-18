.DEFAULT_GOAL := eunit

eunit:
	rm -rf _build/test
	./rebar3 eunit

HEX_CONFIG = $(HOME)/.hex/hex.config
REBAR3_CONFIG = $(HOME)/.config/rebar3/rebar.config

$(HEX_CONFIG):
	mkdir -p `dirname $@`
	echo '{username,<<"'$(HEX_USERNAME)'">>}.' > $@
	echo '{key,<<"'$(HEX_KEY)'">>}.' >> $@

$(REBAR3_CONFIG):
	mkdir -p `dirname $@`
	echo '{plugins, [rebar3_hex]}.' > $@

publish: $(HEX_CONFIG) $(REBAR3_CONFIG)
	echo y | ./rebar3 hex publish
