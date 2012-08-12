-module(hoax_test).

-compile([export_all]).
-import(hoax, [stub/2, expect/2, expect/3, and_return/1, fake/2, unload/1]).

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

fake_should_throw_when_module_exists_test() ->
    ExpectedError = {module_exists, ?REAL_MOD},
    ?assertError(ExpectedError, fake(?REAL_MOD, [])).

unload_should_remove_faked_module_from_vm_test() ->
    fake(?FAKE_MOD, []),
    unload(?FAKE_MOD),
    ?assertMatch({error,nofile}, code:ensure_loaded(?FAKE_MOD)).

fake_should_return_expected_value_when_args_match_test() ->
    fake(?FAKE_MOD, [
            expect(?FAKE_FUN,[arg1,arg2],
                and_return(a_result))
        ]),

    ?assertEqual(a_result, ?FAKE_MOD:?FAKE_FUN(arg1, arg2)),
    unload(?FAKE_MOD).

fake_should_return_default_value_when_args_do_not_match_test() ->
    fake(?FAKE_MOD, [
            expect(?FAKE_FUN,[arg1,arg2],
                and_return(a_result))
        ]),

    ?assertEqual(ok, ?FAKE_MOD:?FAKE_FUN(arg1, not_matching_arg)),
    unload(?FAKE_MOD).
