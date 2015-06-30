-module(hoax_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

stop_should_unload_all_hoaxed_modules_test() ->
    ExpectedResult = hoax_test_module:function_one(1, 2),
    hoax:test(fun() ->
        mock(no_such_module, expect_no_interactions),
        mock(hoax_test_module, [
            ?expect(function_one, ?withArgs([3, 4]), ?andReturn(mocked_return_value_2)),
            ?expect(function_one, ?withArgs([?any, 2]), ?andReturn(mocked_return_value_1))
        ]),

        ?assertEqual(mocked_return_value_1, hoax_test_module:function_one(1, 2)),
        ?assertEqual(mocked_return_value_1, hoax_test_module:function_one(9, 2)),
        ?assertEqual(mocked_return_value_2, hoax_test_module:function_one(3, 4)),
        AllArguments = hoax:arguments(fun hoax_test_module:function_one/2),
        ?assertEqual(3, length(AllArguments)),
        ?assert(lists:member([1,2], AllArguments)),
        ?assert(lists:member([9,2], AllArguments)),
        ?assert(lists:member([3,4], AllArguments))
    end),

    Result = hoax_test_module:function_one(1, 2),
    ?assertEqual(ExpectedResult, Result),

    ?assertMatch({error, nofile}, code:ensure_loaded(no_such_module)).

should_be_able_to_mock_sticky_modules_test() ->
    ExpectedResult = hoax_test_module:function_one(1, 2),
    code:stick_mod(hoax_test_module),
    try
        hoax:test(fun() ->
            mock(hoax_test_module, ?expect(function_one, ?withArgs([1, 2]), ?andReturn(mocked_return_value))),
            ?assertEqual(mocked_return_value, hoax_test_module:function_one(1, 2)),
            ?assert(code:is_sticky(hoax_test_module))
        end),

        Result = hoax_test_module:function_one(1, 2),
        ?assertEqual(ExpectedResult, Result),
        ?assert(code:is_sticky(hoax_test_module))
    after
        code:unstick_mod(hoax_test_module)
    end.

full_stack_fun_expectation_test() ->
    hoax:test(fun() ->
        Expected = 1,
        mock(hoax_test_module, ?expect(function_two, fun(Val) ->
            ?assertEqual(Expected, Val),
            local_return
        end)),

        ?assertEqual(local_return, hoax_test_module:function_two(Expected))
    end).

-define(EXAMPLE_MODULE, module_for_testing_fixtures).

fixture_for_entire_module_without_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/0 || F <- [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2,
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2,
        setup, test_function1, test_function2
    ]],

    {foreach, _, _, Tests} = hoax:fixture(?EXAMPLE_MODULE),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

fixture_for_entire_module_with_setup_and_teardown_calls_setup_and_teardown_test() ->
    {foreach, Setup, Teardown, _} = hoax:fixture(?EXAMPLE_MODULE, setup, teardown),

    erlang:put(setup_called, undefined),
    erlang:put(teardown_called, undefined),
    Setup(),
    Teardown(some_argument),

    ?assertEqual(true, erlang:get(setup_called)),
    ?assertEqual({true, some_argument}, erlang:get(teardown_called)).

fixture_for_entire_module_with_setup_and_teardown_omits_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/0 || F <- [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2,
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2,
        test_function1, test_function2
    ]],

    {foreach, _, _, Tests} = hoax:fixture(?EXAMPLE_MODULE, setup, teardown),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

fixture_with_atom_prefix_without_setup_and_teardown_selects_functions_by_prefix_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/0 || F <- [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2
    ]],

    {foreach, _, _, Tests} = hoax:fixture(?EXAMPLE_MODULE, prefix1),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

fixture_with_string_prefix_without_setup_and_teardown_selects_functions_by_prefix_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/0 || F <- [
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2
    ]],

    {foreach, _, _, Tests} = hoax:fixture(?EXAMPLE_MODULE, "prefix2"),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

fixture_with_atom_prefix_with_setup_and_teardown_selects_functions_by_prefix_omitting_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/0 || F <- [
        prefix1_test_function_1, prefix1_test_function_2
    ]],

    {foreach, _, _, Tests} = hoax:fixture(?EXAMPLE_MODULE, prefix1, prefix1_setup, prefix1_teardown),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

fixture_with_string_prefix_with_setup_and_teardown_selects_functions_by_prefix_omitting_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/0 || F <- [
        prefix2_test_function_1, prefix2_test_function_2
    ]],

    {foreach, _, _, Tests} = hoax:fixture(?EXAMPLE_MODULE, "prefix2", prefix2_setup, prefix2_teardown),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

fixture_with_atom_prefix_with_setup_and_teardown_calls_setup_and_teardown_test() ->
    {foreach, Setup, Teardown, _} = hoax:fixture(?EXAMPLE_MODULE, prefix1, prefix1_setup, prefix1_teardown),

    erlang:put(prefix1_setup_called, undefined),
    erlang:put(prefix1_teardown_called, undefined),
    Setup(),
    Teardown(some_argument),

    ?assertEqual(true, erlang:get(prefix1_setup_called)),
    ?assertEqual({true, some_argument}, erlang:get(prefix1_teardown_called)).

fixture_with_string_prefix_with_setup_and_teardown_calls_setup_and_teardown_test() ->
    {foreach, Setup, Teardown, _} = hoax:fixture(?EXAMPLE_MODULE, "prefix2", prefix2_setup, prefix2_teardown),

    erlang:put(prefix2_setup_called, undefined),
    erlang:put(prefix2_teardown_called, undefined),
    Setup(),
    Teardown(some_argument),

    ?assertEqual(true, erlang:get(prefix2_setup_called)),
    ?assertEqual({true, some_argument}, erlang:get(prefix2_teardown_called)).

parameterized_fixture_for_entire_module_without_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/1 || F <- [
        parameterized_test_function1, parameterized_test_function2,
        prefix1_parameterized_test_function_1, prefix1_parameterized_test_function_2, prefix1_teardown,
        prefix2_parameterized_test_function_1, prefix2_parameterized_test_function_2, prefix2_teardown,
        teardown
    ]],

    {foreach, _, _, [{with, Tests}]} = hoax:parameterized_fixture(?EXAMPLE_MODULE),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

parameterized_fixture_for_entire_module_with_setup_and_teardown_calls_setup_and_teardown_test() ->
    {foreach, Setup, Teardown, _} = hoax:parameterized_fixture(?EXAMPLE_MODULE, setup, teardown),

    erlang:put(setup_called, undefined),
    erlang:put(teardown_called, undefined),
    Setup(),
    Teardown(some_argument),

    ?assertEqual(true, erlang:get(setup_called)),
    ?assertEqual({true, some_argument}, erlang:get(teardown_called)).

parameterized_fixture_for_entire_module_with_setup_and_teardown_omits_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/1 || F <- [
        parameterized_test_function1, parameterized_test_function2,
        prefix1_parameterized_test_function_1, prefix1_parameterized_test_function_2, prefix1_teardown,
        prefix2_parameterized_test_function_1, prefix2_parameterized_test_function_2, prefix2_teardown
    ]],

    {foreach, _, _, [{with, Tests}]} = hoax:parameterized_fixture(?EXAMPLE_MODULE, setup, teardown),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

parameterized_fixture_with_atom_prefix_without_setup_and_teardown_selects_functions_by_prefix_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/1 || F <- [
        prefix1_parameterized_test_function_1, prefix1_parameterized_test_function_2, prefix1_teardown
    ]],

    {foreach, _, _, [{with, Tests}]} = hoax:parameterized_fixture(?EXAMPLE_MODULE, prefix1),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

parameterized_fixture_with_string_prefix_without_setup_and_teardown_selects_functions_by_prefix_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/1 || F <- [
        prefix2_parameterized_test_function_1, prefix2_parameterized_test_function_2, prefix2_teardown
    ]],

    {foreach, _, _, [{with, Tests}]} = hoax:parameterized_fixture(?EXAMPLE_MODULE, "prefix2"),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

parameterized_fixture_with_atom_prefix_with_setup_and_teardown_selects_functions_by_prefix_omitting_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/1 || F <- [
        prefix1_parameterized_test_function_1, prefix1_parameterized_test_function_2
    ]],

    {foreach, _, _, [{with, Tests}]} = hoax:parameterized_fixture(?EXAMPLE_MODULE, prefix1, prefix1_setup, prefix1_teardown),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

parameterized_fixture_with_string_prefix_with_setup_and_teardown_selects_functions_by_prefix_omitting_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [ fun ?EXAMPLE_MODULE:F/1 || F <- [
        prefix2_parameterized_test_function_1, prefix2_parameterized_test_function_2
    ]],

    {foreach, _, _, [{with, Tests}]} = hoax:parameterized_fixture(?EXAMPLE_MODULE, "prefix2", prefix2_setup, prefix2_teardown),

    ?assertEqual(ExpectedSortedFunctions, lists:sort(Tests)).

parameterized_fixture_with_atom_prefix_with_setup_and_teardown_calls_setup_and_teardown_test() ->
    {foreach, Setup, Teardown, _} = hoax:parameterized_fixture(?EXAMPLE_MODULE, prefix1, prefix1_setup, prefix1_teardown),

    erlang:put(prefix1_setup_called, undefined),
    erlang:put(prefix1_teardown_called, undefined),
    Setup(),
    Teardown(some_argument),

    ?assertEqual(true, erlang:get(prefix1_setup_called)),
    ?assertEqual({true, some_argument}, erlang:get(prefix1_teardown_called)).

parameterized_fixture_with_string_prefix_with_setup_and_teardown_calls_setup_and_teardown_test() ->
    {foreach, Setup, Teardown, _} = hoax:parameterized_fixture(?EXAMPLE_MODULE, "prefix2", prefix2_setup, prefix2_teardown),

    erlang:put(prefix2_setup_called, undefined),
    erlang:put(prefix2_teardown_called, undefined),
    Setup(),
    Teardown(some_argument),

    ?assertEqual(true, erlang:get(prefix2_setup_called)),
    ?assertEqual({true, some_argument}, erlang:get(prefix2_teardown_called)).
