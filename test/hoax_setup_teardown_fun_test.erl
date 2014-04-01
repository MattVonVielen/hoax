-module(hoax_setup_teardown_fun_test).

-include_lib("hoax/include/hoax.hrl").
-include_lib("eunit/include/eunit.hrl").


?HOAX_FIXTURE(fun setup/0,
              fun teardown/1).

setup() ->
    ok.

teardown(_) ->
    ok.

a_test() ->
    ok.
