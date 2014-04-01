-import(hoax, [mock/2, stub/3]).

-define(HOAX_FIXTURE(Setup, Teardown),
    hoax_fixture_test_() ->
        {foreach,
            fun() -> hoax:start(), Setup() end,
            fun(X) -> Teardown(X), hoax:stop() end,
            hoax_macro:test_list(?MODULE, Setup, Teardown)
        }
).

-define(HOAX_FIXTURE,
    ?HOAX_FIXTURE(fun() -> ok end, fun(_) -> ok end)
).

-define(HOAX_FIXTURE_WITH_ARG(Setup, Teardown),
    hoax_fixture_test_() ->
        {foreach,
            fun() -> hoax:start(), Setup() end,
            fun(X) -> Teardown(X), hoax:stop() end,
            hoax_macro:test_list(?MODULE, Setup, Teardown)
        }
).

-define(HOAX_FIXTURE_WITH_ARG,
    ?HOAX_FIXTURE_WITH_ARG(fun() -> ok end, fun(_) -> ok end)
).

-define(verifyAll,
    ((fun () ->
                    case (hoax_tab:unmet_expectations()) of
                        []  -> ok;
                        Unmet -> erlang:error({unmet_expectations,
                                [{module, ?MODULE},
                                    {line, ?LINE},
                                    {expected, Unmet}
                                ]})
                end
        end)())).

-define(expect(Func, Args), {Func, Args}).
-define(expect(Func, Args, Action_or_Count), {Func, Args, Action_or_Count}).
-define(expect(Func, Args, Action, Count), {Func, Args, Action, Count}).

-define(withArgs(Args), Args).

-define(andReturn(Val), {return, Val}).
-define(andThrow(Val), {throw, Val}).
-define(andError(Val), {error, Val}).
-define(andExit(Val), {exit, Val}).

-define(times(N), N).
-define(once, 1).
-define(twice, 2).

-define(any, '_').
