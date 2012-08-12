-define(HOAX_API, [
        stub/1,
        stub/2,
        stub_a/2,
        stub_a/3,
        fake/2,
        expect/2,
        expect/3,
        and_return/1,
        and_throw/1,
        unload/0,
        unload/1
    ]).

-ifdef(IN_HOAX_ERL).
-export(?HOAX_API).
-ignore_xref(?HOAX_API).
-else.
-import(hoax, ?HOAX_API).
-endif.

-define(HF_NAME, hoax_fixture_test_).
-define(HF_EMPTY_SETUP, fun() -> ok end).
-define(HF_EMPTY_TEARDOWN, fun(_) -> ok end).
-define(HF_TEST_DECL, fun ?MODULE:F/0).
-define(HF_TEST_DECL_WITH_ARG, {with, [fun ?MODULE:F/1]}).

-define(HF_FIXTURE_BASE(Setup, Teardown, TestArity, TestDecl),
    ?HF_NAME() ->
        {foreach,
            fun() -> Setup() end,
            fun(X) -> Teardown(X), hoax:unload() end,
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

