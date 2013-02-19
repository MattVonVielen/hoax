-module(hoax_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

?HOAX_FIXTURE.

stop_should_unload_all_hoaxed_modules() ->
    fake(no_such_module, []),
    stub(hoax_test_module, []),

    stop(),

    IsFakeLoaded = code:ensure_loaded(no_such_module),
    ?assertMatch({error,nofile}, IsFakeLoaded),

    IsRealLoaded = code:ensure_loaded(hoax_test_module),
    ?assertMatch({module, hoax_test_module}, IsRealLoaded),

    Result = hoax_test_module:function_one(1, 2),
    ?assertEqual({function_one, 1, 2}, Result).

%%%' =================================
%%%  Preconditions - stub
%%%  =================================

stub_should_throw_when_module_cannot_be_loaded() ->
    ExpectedError = {no_such_module_to_mock, no_such_module},
    ?assertError(ExpectedError, stub(no_such_module, [])).

stub_should_throw_when_module_does_not_have_expected_function() ->
    ExpectedError = {no_such_function_to_mock, {no_such_function, 0}},
    ?assertError(ExpectedError,
                 stub(hoax_test_module, [
                                         expect(no_such_function, [])
                                        ])).

%%%. =================================

%%%' =================================
%%%  Preconditions - stub_behaviour
%%%  =================================

stub_behaviour_should_throw_when_behaviour_cannot_be_loaded() ->
    ExpectedError = {no_such_behaviour_to_mock, no_such_module},
    ?assertError(ExpectedError, stub(no_such_module, name_of_mock)).

stub_behaviour_should_throw_when_behaviour_arg_is_not_a_behaviour() ->
    ExpectedError = {not_a_behaviour, hoax_test_module},
    ?assertError(ExpectedError, stub(hoax_test_module, name_of_mock)).

stub_behaviour_should_throw_when_output_module_exists() ->
    ExpectedError = {module_exists, hoax_test_module},
    ?assertError(ExpectedError, stub(hoax_test_behaviour, hoax_test_module)).

stub_behaviour_should_throw_when_behaviour_does_not_have_expected_callback() ->
    ExpectedError = {no_such_function_to_mock, {no_such_function, 0}},
    ?assertError(ExpectedError,
                 stub(hoax_test_behaviour, name_of_mock, [
                                         expect(no_such_function, [])
                                        ])).

%%%. =================================

%%%' =================================
%%%  Preconditions - fake
%%%  =================================

fake_should_throw_when_module_exists() ->
    ExpectedError = {module_exists, hoax_test_module},
    ?assertError(ExpectedError, fake(hoax_test_module, [])).

%%%. =================================

%%%' =================================
%%%  Preconditions - mock
%%%  =================================

mock_should_throw_when_module_cannot_be_loaded() ->
    ExpectedError = {no_such_module_to_mock, no_such_module},
    ?assertError(ExpectedError, mock(no_such_module, [])).

mock_should_throw_when_module_does_not_have_expected_function() ->
    ExpectedError = {no_such_function_to_mock, {no_such_function, 0}},
    ?assertError(ExpectedError,
                 mock(hoax_test_module, [
                                         expect(no_such_function, [])
                                        ])).

%%%. =================================

%%%' =================================
%%%  Preconditions - mock_behaviour
%%%  =================================

mock_behaviour_should_throw_when_behaviour_cannot_be_loaded() ->
    ExpectedError = {no_such_behaviour_to_mock, no_such_module},
    ?assertError(ExpectedError, mock(no_such_module, name_of_mock)).

mock_behaviour_should_throw_when_behaviour_arg_is_not_a_behaviour() ->
    ExpectedError = {not_a_behaviour, hoax_test_module},
    ?assertError(ExpectedError, mock(hoax_test_module, name_of_mock)).

mock_behaviour_should_throw_when_output_module_exists() ->
    ExpectedError = {module_exists, hoax_test_module},
    ?assertError(ExpectedError, mock(hoax_test_behaviour, hoax_test_module)).

mock_behaviour_should_throw_when_behaviour_does_not_have_expected_callback() ->
    ExpectedError = {no_such_function_to_mock, {no_such_function, 0}},
    ?assertError(ExpectedError,
                 mock(hoax_test_behaviour, name_of_mock, [
                                         expect(no_such_function, [])
                                        ])).

%%%. =================================

%%%' =================================
%%%  Expected call to return value
%%%  =================================

mock_should_return_expected_value_when_args_match() ->
    mock(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_return(a_result))
                           ]),

    Result = hoax_test_module:function_one(arg1, arg2),

    ?assertEqual(a_result, Result).

mock_should_return_discrete_expected_values_for_appropriate_args_regardless_of_call_order() ->
    mock(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_return(result_for_args_1_and_2)),
                            expect(function_one,[arg3,arg4],
                                   and_return(result_for_args_3_and_4))
                           ]),

    ?assertEqual(result_for_args_3_and_4, hoax_test_module:function_one(arg3, arg4)),
    ?assertEqual(result_for_args_1_and_2, hoax_test_module:function_one(arg1, arg2)).

mock_behaviour_should_return_expected_value_when_args_match() ->
    mock(hoax_test_behaviour, name_of_mock, [
                            expect(callback_one,[arg1],
                                   and_return(a_result))
                           ]),

    Result = name_of_mock:callback_one(arg1),

    ?assertEqual(a_result, Result).

stub_should_return_expected_value_when_args_match() ->
    stub(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_return(a_result))
                           ]),

    Result = hoax_test_module:function_one(arg1, arg2),

    ?assertEqual(a_result, Result).

stub_behaviour_should_return_expected_value_when_args_match() ->
    stub(hoax_test_behaviour, name_of_mock, [
                            expect(callback_one,[arg1],
                                   and_return(a_result))
                           ]),

    Result = name_of_mock:callback_one(arg1),

    ?assertEqual(a_result, Result).

fake_should_return_expected_value_when_args_match() ->
    fake(no_such_module, [
                          expect(no_such_function,[arg1,arg2],
                                 and_return(a_result))
                         ]),

    Result = no_such_module:no_such_function(arg1, arg2),

    ?assertEqual(a_result, Result).

%%%. =================================

%%%' =================================
%%%  Expected call to throw error
%%%  =================================

mock_should_throw_expected_value_when_args_match() ->
    mock(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_throw(an_error))
                           ]),

    ?assertError(an_error,
                 hoax_test_module:function_one(arg1, arg2)).

mock_behaviour_should_throw_expected_value_when_args_match() ->
    mock(hoax_test_behaviour, name_of_mock, [
                            expect(callback_one,[arg1],
                                   and_throw(an_error))
                           ]),

    ?assertError(an_error,
                 name_of_mock:callback_one(arg1)).

stub_should_throw_expected_value_when_args_match() ->
    stub(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_throw(an_error))
                           ]),

    ?assertError(an_error,
                 hoax_test_module:function_one(arg1, arg2)).

stub_behaviour_should_throw_expected_value_when_args_match() ->
    stub(hoax_test_behaviour, name_of_mock, [
                            expect(callback_one,[arg1],
                                   and_throw(an_error))
                           ]),

    ?assertError(an_error,
                 name_of_mock:callback_one(arg1)).

fake_should_throw_expected_value_when_args_match() ->
    fake(no_such_module, [
                          expect(no_such_function,[arg1,arg2],
                                 and_throw(an_error))
                         ]),

    ?assertError(an_error,
                 no_such_module:no_such_function(arg1, arg2)).

%%%. =================================

%%%' =================================
%%%  Expected call, argument mismatch
%%%  =================================

mock_should_throw_when_args_do_not_match() ->
    mock(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_return(a_result))
                           ]),

    ExpectedError = {unexpected_arguments, {hoax_test_module,
            function_one, [arg1, not_matching_arg]}},
    ?assertError(ExpectedError, hoax_test_module:function_one(arg1, not_matching_arg)).

mock_behaviour_should_throw_when_args_do_not_match() ->
    mock(hoax_test_behaviour, name_of_mock, [
                            expect(callback_one,[arg1],
                                   and_return(a_result))
                           ]),

    ExpectedError = {unexpected_arguments, {name_of_mock, callback_one, [not_matching_arg]}},
    ?assertError(ExpectedError, name_of_mock:callback_one(not_matching_arg)).

stub_should_throw_when_args_do_not_match() ->
    stub(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_return(a_result))
                           ]),

    ExpectedError = {unexpected_arguments, {hoax_test_module, function_one, [arg1, not_matching_arg]}},
    ?assertError(ExpectedError, hoax_test_module:function_one(arg1, not_matching_arg)).

stub_behaviour_should_throw_when_args_do_not_match() ->
    stub(hoax_test_behaviour, name_of_mock, [
                            expect(callback_one,[arg1],
                                   and_return(a_result))
                           ]),

    ExpectedError = {unexpected_arguments, {name_of_mock, callback_one, [not_matching_arg]}},
    ?assertError(ExpectedError, name_of_mock:callback_one(not_matching_arg)).

fake_should_throw_when_args_do_not_match() ->
    fake(no_such_module, [expect(no_such_function,[arg1,arg2], and_return(a_result))]),

    ExpectedError = {unexpected_arguments, {no_such_module, no_such_function, [arg1, not_matching_arg]}},
    ?assertError(ExpectedError, no_such_module:no_such_function(arg1, not_matching_arg)).

%%%. =================================

%%%' =================================
%%%  Unexpected call on mock
%%%  =================================

mock_should_throw_when_unexpected_call() ->
    mock(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_return(a_result))
                           ]),

    ExpectedError = {unexpected_invocation, {hoax_test_module, function_two, [arg1]}},
    ?assertError(ExpectedError, hoax_test_module:function_two(arg1)).

mock_behaviour_should_throw_when_unexpected_call() ->
    mock(hoax_test_behaviour, name_of_mock, [
                            expect(callback_one,[arg1],
                                   and_return(a_result))
                           ]),

    ExpectedError = {unexpected_invocation, {name_of_mock, callback_two, [arg1, arg2]}},
    ?assertError(ExpectedError, name_of_mock:callback_two(arg1, arg2)).

stub_should_return_default_value_when_unexpected_call() ->
    stub(hoax_test_module, [
                            expect(function_one,[arg1,arg2],
                                   and_return(a_result))
                           ]),

    Result = hoax_test_module:function_two(arg1),

    ?assertEqual(ok, Result).

stub_behaviour_should_return_default_value_when_unexpected_call() ->
    stub(hoax_test_behaviour, name_of_mock, [
                            expect(callback_one,[arg1],
                                   and_return(a_result))
                           ]),

    Result = name_of_mock:callback_two(arg1, arg2),

    ?assertEqual(ok, Result).

%%% vim: set foldmarker=%%%',%%%. foldmethod=marker:
