-module(hoax_macro).

-export([
         test_list/3

        ]).

-ifdef(TEST).
-export([
         find_fun_name/2
        ]).
-endif.

test_list(Module, Setup, Teardown) when is_function(Setup) andalso is_function(Teardown)->
    SetupName = find_fun_name(Module, Setup),
    TeardownName = find_fun_name(Module, Teardown),
    [ {Module, F} ||
        {F, 0} <- Module:module_info(exports),
        F =/= hoax_fixture_test_,
        F =/= module_info,
        F =/= test,
        F =/= SetupName,
        F =/= TeardownName
    ].

find_fun_name(Module, Fun) ->
    FunProps = erlang:fun_info(Fun),
    case lists:keyfind(module, 1, FunProps) of
        {module, Module} ->
            {name, FunName} = lists:keyfind(name, 1, FunProps),
            FunName;
        _AnotherModule ->
            not_exported
    end.
