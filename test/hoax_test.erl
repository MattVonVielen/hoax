-module(hoax_test).

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
