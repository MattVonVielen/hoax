-module(hoax_module_tests).

-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

generate_test() ->
    Forms = hoax_module:generate(m, [{f,2},{g,1},{h,0}]),

    {ok, ExpectedModule} = file:read_file("testdata/hoax_module/output/m.erl"),
    ?assertEqual(binary_to_list(ExpectedModule), erl_prettypr:format(erl_syntax:form_list(Forms))).
