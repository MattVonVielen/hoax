-module(hoax_macro_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

hoax_macro_test_list_test() ->
    Result = hoax_macro:test_list(?MODULE, fun hoax_macro_test:setup/0, (fun hoax_macro_test:teardown/1) ),
    ?assertEqual([
                  {?MODULE, hoax_macro_test_list_test},
                  {?MODULE, hoax_macro_find_fun_name_test_}
                 ], Result).

hoax_macro_find_fun_name_test_() ->
    [
     ?_assertEqual(setup, hoax_macro:find_fun_name(?MODULE, fun hoax_macro_test:setup/0 )),
     ?_assertEqual(teardown, hoax_macro:find_fun_name(?MODULE, fun hoax_macro_test:teardown/1 ))
    ].

setup() ->
    ok.

teardown(_) ->
    ok.
