-module(hoax_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

stop_should_unload_all_hoaxed_modules_test() ->
    ExpectedResult = hoax_test_module:function_one(1, 2),
    hoax:start(),

    try
        mock(no_such_module, expect_no_interactions),
        mock(hoax_test_module, ?expect(function_one, ?withArgs([1, 2]), ?andReturn(mocked_return_value))),

        ?assertEqual(mocked_return_value, hoax_test_module:function_one(1, 2))
    after
        hoax:stop()
    end,

    Result = hoax_test_module:function_one(1, 2),
    ?assertEqual(ExpectedResult, Result),

    ?assertMatch({error,nofile}, code:ensure_loaded(no_such_module)).

should_be_able_to_mock_sticky_modules_test() ->
    ExpectedResult = hoax_test_module:function_one(1, 2),
    code:stick_mod(hoax_test_module),
    try
        hoax:start(),

        try
            mock(hoax_test_module, ?expect(function_one, ?withArgs([1, 2]), ?andReturn(mocked_return_value))),
            ?assertEqual(mocked_return_value, hoax_test_module:function_one(1, 2)),
            ?assert(code:is_sticky(hoax_test_module))
        after
            hoax:stop()
        end,
        Result = hoax_test_module:function_one(1, 2),
        ?assertEqual(ExpectedResult, Result),
        ?assert(code:is_sticky(hoax_test_module))
    after
        code:unstick_mod(hoax_test_module)
    end.

full_stack_fun_expectation_test() ->
    hoax:start(),
    Expected = 1,
    mock(hoax_test_module, ?expect(function_two, fun(Val) ->
                                  ?assertEqual(Expected, Val),
                                  local_return
                                  end)),

    ?assertEqual(local_return, hoax_test_module:function_two(Expected)),
    hoax:stop().

-define(MOD, module_for_testing_fixtures).

fixture_for_entire_module_without_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2,
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2,
        setup, test_function1, test_function2
    ],

    {foreach, _, _, Tests} = hoax:fixture(?MOD),

    ?assertEqual(ExpectedSortedFunctions, lists:sort([F || {_,F} <- Tests])).

fixture_for_entire_module_with_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2,
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2,
        test_function1, test_function2
    ],

    {foreach, _, _, Tests} = hoax:fixture(?MOD, setup, teardown),

    ?assertEqual(ExpectedSortedFunctions, lists:sort([F || {_,F} <- Tests])).

fixture_for_prefix_without_setup_and_teardown_test() ->
    ExpectedSortedPrefix1Functions = [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2
    ],
    ExpectedSortedPrefix2Functions = [
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2
    ],

    {foreach, _, _, Prefix1Tests} = hoax:fixture(?MOD, prefix1),
    {foreach, _, _, Prefix2Tests} = hoax:fixture(?MOD, prefix2),

    ?assertEqual(ExpectedSortedPrefix1Functions, lists:sort([F || {_,F} <- Prefix1Tests])),
    ?assertEqual(ExpectedSortedPrefix2Functions, lists:sort([F || {_,F} <- Prefix2Tests])),

    {foreach, _, _, Prefix1TestsFromString} = hoax:fixture(?MOD, "prefix1"),
    {foreach, _, _, Prefix2TestsFromString} = hoax:fixture(?MOD, "prefix2"),

    ?assertEqual(Prefix1Tests, Prefix1TestsFromString),
    ?assertEqual(Prefix2Tests, Prefix2TestsFromString).

fixture_for_prefix_with_setup_and_teardown_test() ->
    ExpectedSortedPrefix1Functions = [
        prefix1_test_function_1, prefix1_test_function_2
    ],
    ExpectedSortedPrefix2Functions = [
        prefix2_test_function_1, prefix2_test_function_2
    ],

    {foreach, _, _, Prefix1Tests} = hoax:fixture(?MOD, prefix1, prefix1_setup, prefix1_teardown),
    {foreach, _, _, Prefix2Tests} = hoax:fixture(?MOD, prefix2, prefix2_setup, prefix2_teardown),

    ?assertEqual(ExpectedSortedPrefix1Functions, lists:sort([F || {_,F} <- Prefix1Tests])),
    ?assertEqual(ExpectedSortedPrefix2Functions, lists:sort([F || {_,F} <- Prefix2Tests])),

    {foreach, _, _, Prefix1TestsFromString} = hoax:fixture(?MOD, "prefix1", prefix1_setup, prefix1_teardown),
    {foreach, _, _, Prefix2TestsFromString} = hoax:fixture(?MOD, "prefix2", prefix2_setup, prefix2_teardown),

    ?assertEqual(Prefix1Tests, Prefix1TestsFromString),
    ?assertEqual(Prefix2Tests, Prefix2TestsFromString).
