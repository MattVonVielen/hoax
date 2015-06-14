-module(hoax).

-export([start/0, stop/0]).
-export([mock/2, stub/3]).
-export([fixture/1, fixture/2, fixture/3, fixture/4, test/1]).
-export([parameterized_fixture/1, parameterized_fixture/2, parameterized_fixture/3, parameterized_fixture/4]).
-ignore_xref([mock/2, stub/3]).

%% ===================================================================
%% hoax API
%% ===================================================================

start() ->
    hoax_tab:create().

stop() ->
    lists:foreach(
        fun hoax_code:purge_and_delete/1,
        hoax_tab:delete()).

mock(ModuleName, Expectation) when is_tuple(Expectation) ->
    mock(ModuleName, [Expectation]);
mock(ModuleName, Expectations) ->
    Records = hoax_expect:parse(ModuleName, Expectations),
    Exports = hoax_code:get_export_list(ModuleName, Records),
    hoax_expect:assert_exported(Records, Exports),
    Forms = hoax_module:generate(ModuleName, Exports),
    hoax_code:compile(ModuleName, Forms).

stub(Behaviour, ModuleName, Expectations) ->
    Records = hoax_expect:parse(ModuleName, Expectations),
    Callbacks = hoax_code:get_callback_list(Behaviour, ModuleName),
    hoax_expect:assert_exported(Records, Callbacks),
    Forms = hoax_module:generate(ModuleName, Callbacks),
    hoax_code:compile(ModuleName, Forms).

fixture(Module) ->
    fixture_tuple(Module, 0, fun no_op/0, fun no_op/1, fun default_test_selector/1).

fixture(Module, Prefix) ->
    Selector = fun(F) ->
        default_test_selector(F) andalso prefix_test_selector(Prefix, F)
    end,

    fixture_tuple(Module, 0, fun no_op/0, fun no_op/1, Selector).

fixture(Module, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    Selector = fun(F) ->
        default_test_selector(F) andalso setup_teardown_test_selector(F, Setup, Teardown)
    end,
    fixture_tuple(Module, 0, fun Module:Setup/0, fun Module:Teardown/1, Selector).

fixture(Module, Prefix, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    Selector = fun(F) ->
        default_test_selector(F) andalso setup_teardown_test_selector(F, Setup, Teardown) andalso
            prefix_test_selector(Prefix, F)
    end,
    fixture_tuple(Module, 0, fun Module:Setup/0, fun Module:Teardown/1, Selector).

parameterized_fixture(Module) ->
    fixture_tuple(Module, 1, fun no_op/0, fun no_op/1, fun default_test_selector/1).

parameterized_fixture(Module, Prefix) ->
    Selector = fun(F) ->
        default_test_selector(F) andalso prefix_test_selector(Prefix, F)
    end,

    fixture_tuple(Module, 1, fun no_op/0, fun no_op/1, Selector).

parameterized_fixture(Module, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    Selector = fun(F) ->
        default_test_selector(F) andalso setup_teardown_test_selector(F, Setup, Teardown)
    end,
    fixture_tuple(Module, 1, fun Module:Setup/0, fun Module:Teardown/1, Selector).

parameterized_fixture(Module, Prefix, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    Selector = fun(F) ->
        default_test_selector(F) andalso setup_teardown_test_selector(F, Setup, Teardown) andalso
            prefix_test_selector(Prefix, F)
    end,
    fixture_tuple(Module, 1, fun Module:Setup/0, fun Module:Teardown/1, Selector).

test(Func) when is_function(Func, 0) ->
    try
        start(),
        Func()
    after
        stop()
    end.

default_test_selector(Func) ->
    Name = atom_to_list(Func),
    Func =/= module_info andalso
        Func =/= test andalso
        (not lists:suffix("_test", Name)) andalso
        (not lists:suffix("_test_", Name)).

prefix_test_selector(Prefix, Func) when is_atom(Prefix) ->
    prefix_test_selector(atom_to_list(Prefix), Func);
prefix_test_selector(Prefix, Func) when is_list(Prefix) ->
    Name = atom_to_list(Func),
    lists:prefix(Prefix, Name).

setup_teardown_test_selector(Func, Setup, Teardown) ->
    Func =/= Setup andalso Func =/= Teardown.

no_op() -> ok.
no_op(_) -> ok.

fixture_tuple(Module, Arity, UserSetup, UserTeardown, Selector) ->
    Tests = [fun Module:F/Arity || {F, A} <- Module:module_info(exports), A == Arity, Selector(F) == true],
    TestList = case Arity of
                   0 -> Tests;
                   1 -> [{with, Tests}]
               end,
    {foreach,
        fun() -> start(), UserSetup() end,
        fun(X) -> UserTeardown(X), stop() end,
        TestList
    }.
