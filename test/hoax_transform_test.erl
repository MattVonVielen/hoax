-module(hoax_transform_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

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
