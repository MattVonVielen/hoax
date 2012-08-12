-module(hoax_test).

-compile([export_all]).
-import(hoax, [stub/2, expect/2, expect/3, and_return/1]).

-include_lib("eunit/include/eunit.hrl").

-define(FAKE_MOD, a_nonexistent_module).
-define(FAKE_FUN, nonexistent_function).
-define(REAL_MOD, hoax_test_module).

stub_should_throw_when_module_cannot_be_loaded_test() ->
    ExpectedError = {no_such_module_to_stub, ?FAKE_MOD},
    ?assertError(ExpectedError, stub(?FAKE_MOD, [])).

stub_should_throw_when_module_does_not_have_expected_function_test() ->
    ExpectedError = {no_such_function_to_stub, {?FAKE_FUN, 0}},
    ?assertError(ExpectedError,
        stub(?REAL_MOD, [
                expect(?FAKE_FUN, [])
            ])).

