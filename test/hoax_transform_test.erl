-module(hoax_transform_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

parse_transform_valid_input_test() ->
    {ok, SyntaxTree} = epp_dodger:parse_file("../test/data/hoax_transform_valid_input.txt"),
    {ok, ExpectedText} = file:read_file("../test/data/hoax_transform_valid_input_expected_output.txt"),

    InputForms = erl_syntax:revert_forms(SyntaxTree),

    OutputForms = hoax_transform:parse_transform(InputForms, {ignored}),

    FormList = erl_syntax:form_list(OutputForms),
    OutputText = erl_prettypr:format(FormList, [{paper, 128}, {ribbon, 128}]),
    ?assertEqual(binary_to_list(ExpectedText), OutputText).

parse_transform_bad_input_test() ->
    {ok, SyntaxTree} = epp_dodger:parse_file("../test/data/hoax_transform_bad_input.txt"),
    {ok, ExpectedText} = file:read_file("../test/data/hoax_transform_bad_input_expected_output.txt"),

    InputForms = erl_syntax:revert_forms(SyntaxTree),

    OutputForms = hoax_transform:parse_transform(InputForms, {ignored}),

    FormList = erl_syntax:form_list(OutputForms),
    OutputText = erl_prettypr:format(FormList, [{paper, 128}, {ribbon, 128}]),
    ?assertEqual(binary_to_list(ExpectedText), OutputText).
