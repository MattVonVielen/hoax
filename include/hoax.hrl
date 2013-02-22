-include_lib("hoax/include/hoax_api.hrl").
-import(hoax, ?HOAX_API).

-define(HF_NAME, hoax_fixture_test_).
-define(HF_EMPTY_SETUP, fun() -> ok end).
-define(HF_EMPTY_TEARDOWN, fun(_) -> ok end).
-define(HF_TEST_DECL, {?MODULE, F}).
-define(HF_TEST_DECL_WITH_ARG, {with, [?HF_TEST_DECL]}).

-define(HF_FIXTURE_BASE(Setup, Teardown, TestArity, TestDecl),
    ?HF_NAME() ->
        {foreach,
            fun() -> hoax:start(), Setup() end,
            fun(X) -> Teardown(X), hoax:stop() end,
            [ TestDecl ||
                {F, A} <- ?MODULE:module_info(exports),
                A == TestArity,
                F =/= ?HF_NAME,
                F =/= module_info,
                F =/= test,
                F =/= Setup,
                F =/= Teardown
            ]
        }
).

-define(HOAX_FIXTURE(Setup, Teardown),
    ?HF_FIXTURE_BASE(Setup, Teardown, 0, ?HF_TEST_DECL)
).

-define(HOAX_FIXTURE,
    ?HF_FIXTURE_BASE(?HF_EMPTY_SETUP, ?HF_EMPTY_TEARDOWN, 0, ?HF_TEST_DECL)
).

-define(HOAX_FIXTURE_WITH_ARG(Setup, Teardown),
    ?HF_FIXTURE_BASE(Setup, Teardown, 1, ?HF_TEST_DECL_WITH_ARG)
).

-define(HOAX_FIXTURE_WITH_ARG,
    ?HF_FIXTURE_BASE(?HF_EMPTY_SETUP, ?HF_EMPTY_TEARDOWN, 1,
        ?HF_TEST_DECL_WITH_ARG)
).

-define(verifyAll,
    ((fun () ->
                    case (hoax_tab:unmet_expectations()) of
                        []  -> ok;
                        Unmet -> .erlang:error({unmet_expectations,
                                [{module, ?MODULE},
                                    {line, ?LINE},
                                    {expected, Unmet}
                                ]})
                end
        end)())).
