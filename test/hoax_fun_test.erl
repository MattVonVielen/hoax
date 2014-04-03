-module(hoax_fun_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

should_find_name_in_module_test_() ->
    [
     ?_assertEqual(setup, hoax_fun:find_name_in_module(?MODULE, fun hoax_fun_test:setup/0 )),
     ?_assertEqual(teardown, hoax_fun:find_name_in_module(?MODULE, fun hoax_fun_test:teardown/1 ))
    ].

setup() ->
    ok.

teardown(_) ->
    ok.


should_find_arg_count_for_fun_test() ->
    TestFun = fun(_Arg1, _Arg2, _Arg3) ->
                      ok
              end,
    ?assertEqual(['_', '_', '_'], hoax_fun:create_wildcard_for_args(TestFun)).
