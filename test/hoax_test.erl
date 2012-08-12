-module(hoax_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

?HOAX_FIXTURE.

unload_should_remove_faked_module_from_vm() ->
    fake(no_such_module, []),

    unload(no_such_module),

    Loaded = code:ensure_loaded(no_such_module),

    ?assertMatch({error,nofile}, Loaded).

unload_should_restore_stubbed_module_to_original() ->
    stub(hoax_test_module, []),

    unload(hoax_test_module),

    Loaded = code:ensure_loaded(hoax_test_module),
    ?assertMatch({module, hoax_test_module}, Loaded),

    Result = hoax_test_module:exported_function(1, 2),
    ?assertEqual({exported_function, 1, 2}, Result).

unload_should_throw_when_module_not_hoaxed() ->
    ?assertError({not_hoaxed, hoax_test_module}, unload(hoax_test_module)).

unload_0_should_unload_all_hoaxed_modules() ->
    fake(no_such_module, []),
    stub(hoax_test_module, []),

    unload(),

    IsFakeLoaded = code:ensure_loaded(no_such_module),
    ?assertMatch({error,nofile}, IsFakeLoaded),

    IsRealLoaded = code:ensure_loaded(hoax_test_module),
    ?assertMatch({module, hoax_test_module}, IsRealLoaded),

    Result = hoax_test_module:exported_function(1, 2),
    ?assertEqual({exported_function, 1, 2}, Result).

stub_should_throw_when_module_cannot_be_loaded() ->
    ExpectedError = {no_such_module_to_stub, no_such_module},
    ?assertError(ExpectedError, stub(no_such_module, [])).

stub_should_throw_when_module_does_not_have_expected_function() ->
    ExpectedError = {no_such_function_to_stub, {no_such_function, 0}},
    ?assertError(ExpectedError,
                 stub(hoax_test_module, [
                                         expect(no_such_function, [])
                                        ])).

stub_should_return_expected_value_when_args_match() ->
    stub(hoax_test_module, [
                            expect(exported_function,[arg1,arg2],
                                   and_return(a_result))
                           ]),

    Result = hoax_test_module:exported_function(arg1, arg2),

    ?assertEqual(a_result, Result).

stub_should_throw_expected_value_when_args_match() ->
    stub(hoax_test_module, [
                            expect(exported_function,[arg1,arg2],
                                   and_throw(an_error))
                           ]),

    ?assertError(an_error,
                 hoax_test_module:exported_function(arg1, arg2)).

stub_should_return_default_value_when_args_do_not_match() ->
    stub(hoax_test_module, [
                            expect(exported_function,[arg1,arg2],
                                   and_return(a_result))
                           ]),

    Result = hoax_test_module:exported_function(arg1, not_matching_arg),

    ?assertEqual(ok, Result).

fake_should_throw_when_module_exists() ->
    ExpectedError = {module_exists, hoax_test_module},
    ?assertError(ExpectedError, fake(hoax_test_module, [])).

fake_should_return_expected_value_when_args_match() ->
    fake(no_such_module, [
                                expect(no_such_function,[arg1,arg2],
                                       and_return(a_result))
                               ]),

    Result = no_such_module:no_such_function(arg1, arg2),

    ?assertEqual(a_result, Result).

fake_should_throw_expected_value_when_args_match() ->
    fake(no_such_module, [
                                expect(no_such_function,[arg1,arg2],
                                       and_throw(an_error))
                               ]),

    ?assertError(an_error,
                 no_such_module:no_such_function(arg1, arg2)).

fake_should_return_default_value_when_args_do_not_match() ->
    fake(no_such_module, [
                                expect(no_such_function,[arg1,arg2],
                                       and_return(a_result))
                               ]),

    Result = no_such_module:no_such_function(arg1, not_matching_arg),

    ?assertEqual(ok, Result).

