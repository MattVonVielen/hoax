-import(hoax, [mock/2, stub/3, mock_behaviour/3]).
-include_lib("eunit/include/eunit.hrl").

-ifdef(HOAX_SUPPRESS_DEPRECATION_WARNING).
-define(DEPRECATION_WARNING, warning_ignored).
-else.
-define(DEPRECATION_WARNING,
    ?debugMsg("WARNING: HOAX_FIXTURE macros are deprecated. Write a generator function using hoax:fixture() instead.")
).
-endif.

-define(HOAX_FIXTURE(Setup, Teardown),
    hoax_fixture_test_() ->
        ?DEPRECATION_WARNING,
        hoax:fixture(?MODULE, Setup, Teardown)
).

-define(HOAX_FIXTURE,
    hoax_fixture_test_() ->
        ?DEPRECATION_WARNING,
        hoax:fixture(?MODULE)
).

-define(verifyAll,
    ((fun() ->
        case (hoax_tab:unmet_expectations()) of
            [] -> ok;
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
