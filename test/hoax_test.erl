-module(hoax_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").
-import(hoax, ?HOAX_API).

-define(FAKE_MOD, a_nonexistent_module).
-define(FAKE_FUN, nonexistent_function).
-define(REAL_MOD, hoax_test_module).
-define(REAL_FUN, exported_function).

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

fake_should_throw_expected_value_when_args_match_test() ->
    fake(?FAKE_MOD, [
            expect(?FAKE_FUN,[arg1,arg2],
                and_throw(an_error))
        ]),

    ?assertError(an_error, ?FAKE_MOD:?FAKE_FUN(arg1, arg2)),
    unload(?FAKE_MOD).

fake_should_return_default_value_when_args_do_not_match_test() ->
    fake(?FAKE_MOD, [
            expect(?FAKE_FUN,[arg1,arg2],
                and_return(a_result))
        ]),

    ?assertEqual(ok, ?FAKE_MOD:?FAKE_FUN(arg1, not_matching_arg)),
    unload(?FAKE_MOD).

unload_should_restore_stubbed_module_to_original_test() ->
    stub(?REAL_MOD, []),
    unload(?REAL_MOD),
    ?assertMatch({module, ?REAL_MOD}, code:ensure_loaded(?REAL_MOD)),
    ?assertEqual({?REAL_FUN, 1, 2}, ?REAL_MOD:?REAL_FUN(1,2)).

stub_should_return_expected_value_when_args_match_test() ->
    stub(?REAL_MOD, [
            expect(?REAL_FUN,[arg1,arg2],
                and_return(a_result))
        ]),

    ?assertEqual(a_result, ?REAL_MOD:?REAL_FUN(arg1, arg2)),
    unload(?REAL_MOD).

stub_should_throw_expected_value_when_args_match_test() ->
    stub(?REAL_MOD, [
            expect(?REAL_FUN,[arg1,arg2],
                and_throw(an_error))
        ]),

    ?assertError(an_error, ?REAL_MOD:?REAL_FUN(arg1, arg2)),
    unload(?REAL_MOD).

stub_should_return_default_value_when_args_do_not_match_test() ->
    stub(?REAL_MOD, [
            expect(?REAL_FUN,[arg1,arg2],
                and_return(a_result))
        ]),

    ?assertEqual(ok, ?REAL_MOD:?REAL_FUN(arg1, not_matching_arg)),
    unload(?REAL_MOD).

unload_should_throw_when_module_not_hoaxed_test() ->
    ?assertError({not_hoaxed, ?REAL_MOD}, unload(?REAL_MOD)).

unload_0_should_unload_all_hoaxed_modules_test() ->
    fake(?FAKE_MOD, []),
    stub(?REAL_MOD, []),

    unload(),

    ?assertMatch({error,nofile}, code:ensure_loaded(?FAKE_MOD)),
    ?assertMatch({module, ?REAL_MOD}, code:ensure_loaded(?REAL_MOD)),
    ?assertEqual({?REAL_FUN, 1, 2}, ?REAL_MOD:?REAL_FUN(1,2)).

