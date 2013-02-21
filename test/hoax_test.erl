-module(hoax_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

stop_should_unload_all_hoaxed_modules_test() ->
    start(),

    fake(no_such_module, []),
    mock(hoax_test_module, []),

    stop(),

    IsFakeLoaded = code:ensure_loaded(no_such_module),
    ?assertMatch({error,nofile}, IsFakeLoaded),

    IsRealLoaded = code:ensure_loaded(hoax_test_module),
    ?assertMatch({module, hoax_test_module}, IsRealLoaded),

    Result = hoax_test_module:function_one(1, 2),
    ?assertEqual({function_one, 1, 2}, Result).
