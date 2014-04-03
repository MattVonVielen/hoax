-module(hoax_expect_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("../src/hoax_int.hrl").

assert_exported_should_throw_when_function_not_in_list_test() ->
    ets:new(hoax, [named_table, public]),
    try
        Expectation = #expectation{key = {test_mod, function_0, 0}},
        Exports = [{function_1, 1}],
        ExpectedError = {no_such_function_to_mock, {function_0, 0}},
        ?assertError(ExpectedError,
                     hoax_expect:assert_exported([Expectation], Exports)),
        ?assertEqual([], ets:tab2list(hoax))
    after
        ets:delete(hoax)
    end.

assert_exported_should_return_when_function_in_list_test() ->
    ets:new(hoax, [named_table, public]),
    try
        Expectation = #expectation{key={test_mod, function_0, 0}},
        Exports = [{function_0, 0}, {function_1, 1}],
        ?assertEqual(ok, hoax_expect:assert_exported([Expectation],
                                                     Exports)),
        ?assertEqual([Expectation], ets:tab2list(hoax))
    after
        ets:delete(hoax)
    end.

parse_should_allow_return_action_test() ->
    Expectation = {function_1, [arg1], {return, some_value}, 3},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{key={test_mod, function_1, 1}, args=[arg1],
                                  action={return, some_value}, expected_count=3},
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_throw_action_test() ->
    Expectation = {function_2, [arg1, arg2], {throw, some_error}, 3},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{key={test_mod, function_2, 2}, args=[arg1, arg2],
                                  action={throw, some_error}, expected_count=3},
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_error_action_test() ->
    Expectation = {function_2, [arg1, arg2], {error, some_error}, 3},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{key={test_mod, function_2, 2}, args=[arg1, arg2],
                                  action={error, some_error}, expected_count=3},
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_exit_action_test() ->
    Expectation = {function_2, [arg1, arg2], {exit, some_error}, 3},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{key={test_mod, function_2, 2}, args=[arg1, arg2],
                                  action={exit, some_error}, expected_count=3},
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_action_without_count_test() ->
    Expectation = {function_0, [], {return, some_value}},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{key={test_mod, function_0, 0}, args=[],
                                  action={return, some_value}},
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_no_action_given_with_count_test() ->
    Expectation = {function_0, [], 3},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{key={test_mod, function_0, 0}, args=[],
                                  action=default, expected_count=3},
    ?assertEqual(ExpectedOutput, Result).

parse_should_allow_no_action_given_and_no_count_given_test() ->
    Expectation = {function_0, []},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{key={test_mod, function_0, 0}, args=[],
                                  action=default},
    ?assertEqual(ExpectedOutput, Result).

parse_should_throw_when_expectation_list_is_empty_test() ->
    ExpectedError = {no_expectations_for_mock, test_mod},
    ?assertError(ExpectedError,
                 hoax_expect:parse(test_mod, [])).

parse_should_return_empty_list_when_expectation_is_sentinel_value_test() ->
    ets:new(hoax, [named_table, public]),
    try
        Expectation = #expectation{key={test_mod, undefined, undefined}, expected_count=0},
        ?assertEqual([], hoax_expect:parse(test_mod, expect_no_interactions)),
        ?assertEqual([Expectation], ets:tab2list(hoax))
    after
        ets:delete(hoax)
    end.

parse_should_allow_fun_in_lieu_of_args_test() ->
    Fun = fun(Arg1, Arg2,Arg3) -> {Arg1,Arg2,Arg3} end,
    Expectation = {function_0, Fun},
    [Result] = hoax_expect:parse(test_mod, [Expectation]),
    ExpectedOutput = #expectation{key={test_mod, function_0, 3}, args=['_', '_', '_'],
                                  action={return_fun_result, Fun}},
    ?assertEqual(ExpectedOutput, Result).

parse_should_throw_when_bad_expectation_syntax_test_() ->
    [ {T, ?_assertError({bad_expectation_syntax, E},
                        hoax_expect:parse(test_mod, [E]))}
     || {T, E} <- [
                { "Not a tuple",
                 bad_expectation },
                { "Too few elements",
                 {function_0} },
                { "Too many elements",
                  {function_0, [], {return, some_value}, 0, extra_arg} },
                { "Expectations not a list when defaulting action and count",
                 {function_0, not_a_list} },
                { "Expectations not a list when defaulting action",
                 {function_0, not_a_list, 3} },
                { "Expectations not a list when defaulting count",
                  {function_0, not_a_list, {return, some_value}} },
                { "Expectations not a list when specifying action and count",
                  {function_0, not_a_list, {return, some_value}, 3} },
                { "Arity 3, element 3 neither tuple nor integer",
                  {function_0, [], bad_action_or_count} },
                { "Arity 3, element 3 is invalid action",
                 {function_0, [], {bad_action, action_arg}} },
                { "Arity 4, element 3 neither tuple nor integer",
                  {function_0, [], bad_action, 3} },
                { "Arity 4, element 3 is invalid action",
                  {function_0, [], {bad_action, action_arg}, 3} },
                { "Arity 4, count is an invalid integer",
                 {function_0, [], {return, some_value}, three} }
                ] ].
