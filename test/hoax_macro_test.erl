-module(hoax_macro_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

hoax_macro_test_list_test() ->
    Result = hoax_macro:test_list(?MODULE, fun hoax_macro_test:setup/0, (fun hoax_macro_test:teardown/1) ),
    ?assertEqual([
                  {?MODULE, hoax_macro_test_list_test}
                 ], Result).


setup() ->
    ok.

teardown(_) ->
    ok.

