-module(hoax_invocation_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").
-include("../src/hoax_int.hrl").

?HOAX_FIXTURE.

-define(EXPORTS, [{f,2},{g,1},{h,0}]).

-define(EXPECT(F,Args,Action), #expectation{
        key = {m, F, length(Args)},
        args = Args,
        action = Action
    }).

-define(EXPECT_WITH_COUNT(F, Args, Action, Count), #expectation{
        key = {m, F, length(Args)},
        args = Args,
        expected_count = Count,
        action = Action
    }).

should_return_expected_value_when_args_match() ->
    hoax_tab:insert( ?EXPECT(f,[1,2], {return,a_result}) ),

    Result = hoax_invocation:handle(m, f, [1, 2]),

    ?assertEqual(a_result, Result).

should_return_fun_result_when_expected_args_for_fun() ->
    Fun = fun(Val1, Val2) -> {Val1, Val2} end,

    hoax_tab:insert( ?EXPECT(f,['_','_'], {return_fun_result,Fun}) ),

    Result = hoax_invocation:handle(m, f, [1, 2]),

    ?assertEqual({1,2}, Result).


should_return_expected_value_when_expected_args_include_wildcard_match() ->
    hoax_tab:insert( ?EXPECT(f,[1,'_'], {return,a_result}) ),

    Result = hoax_invocation:handle(m, f, [1, 2]),

    ?assertEqual(a_result, Result).

should_return_discrete_expected_values_for_appropriate_args_regardless_of_call_order() ->
    hoax_tab:insert(?EXPECT(f,[1,2], {return,result_for_args_1_and_2})),
    hoax_tab:insert(?EXPECT(f,[3,4], {return,result_for_args_3_and_4})),

    ?assertEqual(result_for_args_3_and_4, hoax_invocation:handle(m, f, [3, 4])),
    ?assertEqual(result_for_args_1_and_2, hoax_invocation:handle(m, f, [1, 2])).

should_throw_expected_value_when_args_match() ->
    hoax_tab:insert(?EXPECT(f,[1,2], {throw,an_error})),

    ?assertThrow(an_error, hoax_invocation:handle(m, f, [1, 2])).

should_raise_expected_error_when_args_match() ->
    hoax_tab:insert(?EXPECT(f,[1,2], {error,an_error})),

    ?assertError(an_error, hoax_invocation:handle(m, f, [1, 2])).

should_exit_with_expected_error_when_args_match() ->
    hoax_tab:insert(?EXPECT(f,[1,2], {exit,an_error})),

    ?assertExit(an_error, hoax_invocation:handle(m, f, [1, 2])).

should_throw_when_args_do_not_match() ->
    hoax_tab:insert(?EXPECT(f, [1,2], {return, a_result})),

    ExpectedError = {unexpected_arguments, "m:f(1,a)"},
    ?assertError(ExpectedError, hoax_invocation:handle(m, f, [1, a])).

should_throw_when_args_do_not_match_wildcard_pattern() ->
    hoax_tab:insert(?EXPECT(f, ['_',2], {return, a_result})),

    ExpectedError = {unexpected_arguments,  "m:f(1,a)"},
    ?assertError(ExpectedError, hoax_invocation:handle(m, f, [1, a])).

should_throw_when_call_count_equals_expected_count() ->
    hoax_tab:insert(?EXPECT_WITH_COUNT(f, ['_','_'], {return, a_result}, 0)),

    ExpectedError = {too_many_invocations, 1, "m:f('_','_')"},
    ?assertError(ExpectedError, hoax_invocation:handle(m, f, [1, 2])).

should_throw_when_unexpected_call() ->
    hoax_tab:insert(?EXPECT(f, [1,2], {return, a_result})),

    ExpectedError = {unexpected_invocation, "m:g(1)"},
    ?assertError(ExpectedError, hoax_invocation:handle(m, g, [1])).
