-module(hoax_transform_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

parse_transform_test() ->
    {ok, SyntaxTree} = epp_dodger:parse_file("../test/data/hoax_transform_input.txt"),
    {ok, ExpectedText} = file:read_file("../test/data/hoax_transform_expected_output.txt"),

    InputForms = erl_syntax:revert_forms(SyntaxTree),

    OutputForms = hoax_transform:parse_transform(InputForms, {ignored}),

    FormList = erl_syntax:form_list(OutputForms),
    OutputText = erl_prettypr:format(FormList, [{paper, 100}, {ribbon, 100}]),
    ?assertEqual(binary_to_list(ExpectedText), OutputText).
