-module(hoax_expect_test).

-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

expand_expectations_should_throw_when_expected_function_not_in_list_test() ->
    Functions = [{function_1, 1}],
    Expectations = [{{no_such_function, 0}, [], an_action}],
    ExpectedError = {no_such_function_to_mock, {no_such_function, 0}},
    ?assertError(ExpectedError,
                 hoax_expect:expand_expectations(a_module,
                                                 Functions,
                                                 Expectations)).

expand_expectations_should_return_expanded_expectations_when_all_expectations_exist_test() ->
    Functions = [{function_1, 1}],
    Expectations = [
        {{function_1, 1}, [arg1], an_action},
        {{function_1, 1}, [arg2], another_action}
    ],
    ExpectedResult = [
        {a_module, {function_1, 1}, [arg1], an_action},
        {a_module, {function_1, 1}, [arg2], another_action}
    ],
    ?assertEqual(ExpectedResult,
                 hoax_expect:expand_expectations(a_module,
                                                 Functions,
                                                 Expectations)).
