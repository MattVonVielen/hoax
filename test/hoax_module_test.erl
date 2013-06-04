-module(hoax_module_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").
-include("../src/hoax_int.hrl").

?HOAX_FIXTURE.

-define(EXPORTS, [{f,2},{g,1},{h,0}]).

-define(EXPECT(F,Args,Action), #expectation{
        key = {m, F, Args},
        action = Action
    }).

should_return_expected_value_when_args_match() ->
    hoax_tab:insert( ?EXPECT(f,[1,2], {return,a_result}) ),
    hoax_module:compile(m, ?EXPORTS),

    Result = m:f(1, 2),

    ?assertEqual(a_result, Result).

should_return_discrete_expected_values_for_appropriate_args_regardless_of_call_order() ->
    hoax_tab:insert(?EXPECT(f,[1,2], {return,result_for_args_1_and_2})),
    hoax_tab:insert(?EXPECT(f,[3,4], {return,result_for_args_3_and_4})),
    hoax_module:compile(m, ?EXPORTS),

    ?assertEqual(result_for_args_3_and_4, m:f(3, 4)),
    ?assertEqual(result_for_args_1_and_2, m:f(1, 2)).

should_throw_expected_value_when_args_match() ->
    hoax_tab:insert(?EXPECT(f,[1,2], {throw,an_error})),
    hoax_module:compile(m, ?EXPORTS),

    ?assertThrow(an_error, m:f(1, 2)).

should_raise_expected_error_when_args_match() ->
    hoax_tab:insert(?EXPECT(f,[1,2], {error,an_error})),
    hoax_module:compile(m, ?EXPORTS),

    ?assertError(an_error, m:f(1, 2)).

should_exit_with_expected_error_when_args_match() ->
    hoax_tab:insert(?EXPECT(f,[1,2], {exit,an_error})),
    hoax_module:compile(m, ?EXPORTS),

    ?assertExit(an_error, m:f(1, 2)).

should_throw_when_args_do_not_match() ->
    hoax_tab:insert(?EXPECT(f, [1,2], {return, a_result})),
    hoax_module:compile(m, ?EXPORTS),

    ExpectedError = {unexpected_invocation, {m, f, [1, a]}},
    ?assertError(ExpectedError, m:f(1, a)).

should_throw_when_unexpected_call() ->
    hoax_tab:insert(?EXPECT(f, [1,2], {return, a_result})),
    hoax_module:compile(m, ?EXPORTS),

    ExpectedError = {unexpected_invocation, {m, g, [1]}},
    ?assertError(ExpectedError, m:g(1)).

verifyAll_should_throw_verify_error_when_function_not_called() ->
    hoax_tab:insert(?EXPECT(f, [1,2], {return, a_result})),
    hoax_module:compile(m, ?EXPORTS),

    ?assertError({unmet_expectations,[{module,hoax_module_test},
                     {line, _},
                     {expected,[{m,f,[1,2]}]}]}, ?verifyAll).

verifyAll_should_do_nothing_when_all_expectations_satisfied() ->
    hoax_tab:insert(?EXPECT(f, [1,2], {return, a_result})),
    hoax_module:compile(m, ?EXPORTS),

    _Result = m:f(1, 2),

    ?assertNotException(error, {unmet_expectations, _}, ?verifyAll).
