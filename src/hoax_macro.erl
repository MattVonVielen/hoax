-module(hoax_macro).

-export([
         test_list/3
        ]).

test_list(Module, Setup, Teardown) when is_function(Setup) andalso is_function(Teardown)->
    SetupName = hoax_fun:find_name_in_module(Module, Setup),
    TeardownName = hoax_fun:find_name_in_module(Module, Teardown),
    [ {Module, F} ||
        {F, 0} <- Module:module_info(exports),
        F =/= hoax_fixture_test_,
        F =/= module_info,
        F =/= test,
        F =/= SetupName,
        F =/= TeardownName
    ].
