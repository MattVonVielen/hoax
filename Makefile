.DEFAULT_GOAL := eunit

eunit:
	rm -rf _build/test
	./rebar3 eunit
