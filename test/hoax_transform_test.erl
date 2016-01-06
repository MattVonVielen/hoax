-module(hoax_transform_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

parse_transform_valid_input_test() ->
    {ok, SyntaxTree} = epp_dodger:parse_file("../testdata/hoax_transform/valid/input/m.erl"),
    {ok, ExpectedText} = file:read_file("../testdata/hoax_transform/valid/output/m.erl"),

    InputForms = erl_syntax:revert_forms(SyntaxTree),

    OutputForms = hoax_transform:parse_transform(InputForms, {ignored}),

    FormList = erl_syntax:form_list(OutputForms),
    OutputText = erl_prettypr:format(FormList, [{paper, 128}, {ribbon, 128}]),
    %?debugMsg(OutputText),
    ?assertEqual(binary_to_list(ExpectedText), OutputText).

parse_transform_bad_input_test() ->
    {ok, SyntaxTree} = epp_dodger:parse_file("../testdata/hoax_transform/bad/input/m.erl"),
    {ok, ExpectedText} = file:read_file("../testdata/hoax_transform/bad/output/m.erl"),

    InputForms = erl_syntax:revert_forms(SyntaxTree),

    OutputForms = hoax_transform:parse_transform(InputForms, {ignored}),

    FormList = erl_syntax:form_list(OutputForms),
    OutputText = erl_prettypr:format(FormList, [{paper, 128}, {ribbon, 128}]),
    %?debugMsg(OutputText),
    ?assertEqual(binary_to_list(ExpectedText), OutputText).

stop_should_unload_all_hoaxed_modules_with_transform_test() ->
    ExpectedResult = hoax_test_module:function_one(1, 2),
    hoax:test(fun() ->
        mock(no_such_module, expect_no_interactions),
        expect(receive
                       hoax_test_module:function_one(3, 4) -> mocked_return_value_2;
                       hoax_test_module:function_one(_, 2) -> mocked_return_value_1
               end),

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

should_be_able_to_mock_sticky_modules_with_transform_test() ->
    ExpectedResult = hoax_test_module:function_one(1, 2),
    code:stick_mod(hoax_test_module),
    try
        hoax:test(fun() ->
            expect(receive hoax_test_module:function_one(1, 2) -> mocked_return_value end),
            ?assertEqual(mocked_return_value, hoax_test_module:function_one(1, 2)),
            ?assert(code:is_sticky(hoax_test_module))
        end),

        Result = hoax_test_module:function_one(1, 2),
        ?assertEqual(ExpectedResult, Result),
        ?assert(code:is_sticky(hoax_test_module))
    after
        code:unstick_mod(hoax_test_module)
    end.

