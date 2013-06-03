-module(hoax_expect_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("../src/hoax_int.hrl").

assert_exported_should_throw_when_function_not_in_list_test() ->
    Expectation = #expectation{
        key = {test_mod, function_0, []},
        action = default
    },
    Exports = [{function_1, 1}],
    ExpectedError = {no_such_function_to_mock, {function_0, 0}},
    ?assertError(ExpectedError,
                 hoax_expect:assert_exported([Expectation], Exports)).

assert_exported_should_return_when_function_in_list_test() ->
    Expectation = #expectation{
        key = {test_mod, function_0, []},
        action = default
    },
    Exports = [{function_0, 0}, {function_1, 1}],
    ?assertEqual(ok, hoax_expect:assert_exported([Expectation], Exports)).

parse_should_allow_no_action_given_test() ->
    Expectation = {function_0, []},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{
        key = {test_mod, function_0, []},
        action = default
    },
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_return_action_test() ->
    Expectation = {function_1, [arg1], {return, some_value}},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{
        key = {test_mod, function_1, [arg1]},
        action = {return, some_value}
    },
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_throw_action_test() ->
    Expectation = {function_2, [arg1, arg2],
		   {throw, some_error}},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{
        key = {test_mod, function_2, [arg1, arg2]},
        action = {throw, some_error}
    },
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_error_action_test() ->
    Expectation = {function_2, [arg1, arg2],
		   {error, some_error}},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{
        key = {test_mod, function_2, [arg1, arg2]},
        action = {error, some_error}
    },
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_exit_action_test() ->
    Expectation = {function_2, [arg1, arg2],
		   {exit, some_error}},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{
        key = {test_mod, function_2, [arg1, arg2]},
        action = {exit, some_error}
    },
    ?assertEqual(ExpectedOutput, Result).

parse_should_throw_when_args_not_a_list_and_no_action_test() ->
    Expectation = {function_0, not_a_list},
    ExpectedError = {bad_expectation_syntax, Expectation},
    ?assertError(ExpectedError,
                 hoax_expect:parse(test_mod, [Expectation])).

parse_should_throw_when_args_not_a_list_and_has_legal_action_test() ->
    Expectation = {function_0, not_a_list, return},
    ExpectedError = {bad_expectation_syntax, Expectation},
    ?assertError(ExpectedError,
                 hoax_expect:parse(test_mod, [Expectation])).

parse_should_throw_when_bad_element_count_test() ->
    Expectation = {function_0, [], {return, some_value}, extra_arg},
    ExpectedError = {bad_expectation_syntax, Expectation},
    ?assertError(ExpectedError,
                 hoax_expect:parse(test_mod, [Expectation])).

parse_should_throw_when_bad_action_test() ->
    Expectation = {function_0, [], bad_action},
    ExpectedError = {bad_expectation_syntax, Expectation},
    ?assertError(ExpectedError,
                 hoax_expect:parse(test_mod, [Expectation])).

parse_should_throw_when_illegal_action_test() ->
    Expectation = {function_0, [], {bad_action, action_arg }},
    ExpectedError = {bad_expectation_syntax, Expectation},
    ?assertError(ExpectedError,
                 hoax_expect:parse(test_mod, [Expectation])).

