-module(hoax_fun_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

should_find_arg_count_for_fun_test() ->
    TestFun = fun(_Arg1, _Arg2, _Arg3) ->
                      ok
              end,
    ?assertEqual(['_', '_', '_'], hoax_fun:create_wildcard_for_args(TestFun)).
