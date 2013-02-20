-module(hoax_expect_test).

-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

validate_should_allow_no_action_given_test() ->
    Expectation = {function_0, []},
    [Result] = hoax_expect:validate([Expectation]),
    ?assertEqual({function_0,0}, Result).

validate_should_allow_return_action_test() ->
    Expectation = {function_1, [arg1], {return, some_value}},
    [Result] = hoax_expect:validate([Expectation]),
    ?assertEqual({function_1,1}, Result).

validate_should_allow_throw_action_test() ->
    Expectation = {function_2, [arg1,arg2], {throw, some_error}},
    [Result] = hoax_expect:validate([Expectation]),
    ?assertEqual({function_2,2}, Result).

validate_should_throw_when_args_not_a_list_test() ->
    Expectation = {function_0, not_a_list},
    ExpectedError = {bad_expectation_syntax, Expectation},
    ?assertError(ExpectedError,
                 hoax_expect:validate([Expectation])).

validate_should_throw_when_bad_element_count_test() ->
    Expectation = {function_0, [], {return, some_value}, extra_arg},
    ExpectedError = {bad_expectation_syntax, Expectation},
    ?assertError(ExpectedError,
                 hoax_expect:validate([Expectation])).

validate_should_throw_when_bad_action_test() ->
    Expectation = {function_0, [], bad_action},
    ExpectedError = {bad_expectation_syntax, Expectation},
    ?assertError(ExpectedError,
                 hoax_expect:validate([Expectation])).

assert_exported_should_throw_when_function_not_in_list_test() ->
    Function = {function_0, 0},
    Exports = [{function_1, 1}],
    ExpectedError = {no_such_function_to_mock, Function},
    ?assertError(ExpectedError,
                 hoax_expect:assert_exported([Function], Exports)).

assert_exported_should_return_when_function_in_list_test() ->
    Function = {function_0, 0},
    Exports = [{function_0, 0}, {function_1, 1}],
    ?assertEqual(ok, hoax_expect:assert_exported([Function], Exports)).
