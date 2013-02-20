-module(hoax_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

?HOAX_FIXTURE.

stop_should_unload_all_hoaxed_modules() ->
    fake(no_such_module, []),
    mock(hoax_test_module, []),

    stop(),

    IsFakeLoaded = code:ensure_loaded(no_such_module),
    ?assertMatch({error,nofile}, IsFakeLoaded),

    IsRealLoaded = code:ensure_loaded(hoax_test_module),
    ?assertMatch({module, hoax_test_module}, IsRealLoaded),

    Result = hoax_test_module:function_one(1, 2),
    ?assertEqual({function_one, 1, 2}, Result).

%%%' =================================
%%%  Expected call to return value
%%%  =================================

mock_should_return_expected_value_when_args_match() ->
    mock(hoax_test_module, [ {function_one,[arg1,arg2], {return,a_result}} ]),

    Result = hoax_test_module:function_one(arg1, arg2),

    ?assertEqual(a_result, Result).

mock_should_return_discrete_expected_values_for_appropriate_args_regardless_of_call_order() ->
    mock(hoax_test_module, [ {function_one,[arg1,arg2], {return,result_for_args_1_and_2}},
                             {function_one,[arg3,arg4], {return,result_for_args_3_and_4}} ]),

    ?assertEqual(result_for_args_3_and_4, hoax_test_module:function_one(arg3, arg4)),
    ?assertEqual(result_for_args_1_and_2, hoax_test_module:function_one(arg1, arg2)).

stub_behaviour_should_return_expected_value_when_args_match() ->
    stub(hoax_test_behaviour, name_of_mock, [ {callback_one,[arg1], {return,a_result}} ]),

    Result = name_of_mock:callback_one(arg1),

    ?assertEqual(a_result, Result).

fake_should_return_expected_value_when_args_match() ->
    fake(no_such_module, [ {no_such_function,[arg1,arg2], {return,a_result}} ]),

    Result = no_such_module:no_such_function(arg1, arg2),

    ?assertEqual(a_result, Result).

%%%. =================================

%%%' =================================
%%%  Expected call to throw error
%%%  =================================

mock_should_throw_expected_value_when_args_match() ->
    mock(hoax_test_module, [ {function_one,[arg1,arg2], {throw,an_error}} ]),

    ?assertError(an_error,
                 hoax_test_module:function_one(arg1, arg2)).

stub_behaviour_should_throw_expected_value_when_args_match() ->
    stub(hoax_test_behaviour, name_of_mock, [ {callback_one,[arg1], {throw,an_error}} ]),

    ?assertError(an_error,
                 name_of_mock:callback_one(arg1)).

fake_should_throw_expected_value_when_args_match() ->
    fake(no_such_module, [ {no_such_function,[arg1,arg2], {throw,an_error}} ]),

    ?assertError(an_error,
                 no_such_module:no_such_function(arg1, arg2)).

%%%. =================================

%%%' =================================
%%%  Expected call, argument mismatch
%%%  =================================

mock_should_throw_when_args_do_not_match() ->
    mock(hoax_test_module, [ {function_one,[arg1,arg2], {return,a_result}} ]),

    ExpectedError = {unexpected_arguments, {hoax_test_module,
            function_one, [arg1, not_matching_arg]}},
    ?assertError(ExpectedError, hoax_test_module:function_one(arg1, not_matching_arg)).

stub_behaviour_should_throw_when_args_do_not_match() ->
    stub(hoax_test_behaviour, name_of_mock, [ {callback_one,[arg1], {return,a_result}} ]),

    ExpectedError = {unexpected_arguments, {name_of_mock, callback_one, [not_matching_arg]}},
    ?assertError(ExpectedError, name_of_mock:callback_one(not_matching_arg)).

fake_should_throw_when_args_do_not_match() ->
    fake(no_such_module, [{no_such_function,[arg1,arg2], {return,a_result}}]),

    ExpectedError = {unexpected_arguments, {no_such_module, no_such_function, [arg1, not_matching_arg]}},
    ?assertError(ExpectedError, no_such_module:no_such_function(arg1, not_matching_arg)).

%%%. =================================

%%%' =================================
%%%  Unexpected call on mock
%%%  =================================

mock_should_throw_when_unexpected_call() ->
    mock(hoax_test_module, [ {function_one,[arg1,arg2], {return,a_result}} ]),

    ExpectedError = {unexpected_invocation, {hoax_test_module, function_two, [arg1]}},
    ?assertError(ExpectedError, hoax_test_module:function_two(arg1)).

%%% vim: set foldmarker=%%%',%%%. foldmethod=marker:
