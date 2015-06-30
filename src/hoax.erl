-module(hoax).

-export([start/0, stop/0]).
-export([mock/2, stub/3, arguments/1]).
-export([fixture/1, fixture/2, fixture/3, fixture/4, test/1]).
-export([parameterized_fixture/1, parameterized_fixture/2, parameterized_fixture/3, parameterized_fixture/4]).
-ignore_xref([mock/2, stub/3]).

-include("hoax_int.hrl").

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

arguments(F) when is_function(F) ->
    Info = erlang:fun_info(F),
    Module = proplists:get_value(module, Info),
    Name = proplists:get_value(name, Info),
    Arity = proplists:get_value(arity, Info),
    arguments({Module, Name, Arity});
arguments({_Module, _Name, _Arity} = Key) ->
    lists:append([Args || #expectation{actual_args=Args} <- hoax_tab:lookup_expectations(Key)]).

fixture(Module) ->
    fixture_tuple(Module, 0, fun default_test_selector/1).

fixture(Module, Prefix) ->
    fixture_tuple(Module, 0, prefix_test_selector(Prefix)).

fixture(Module, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    fixture_tuple(Module, 0, Setup, Teardown, setup_teardown_test_selector(Setup, Teardown)).

fixture(Module, Prefix, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    fixture_tuple(Module, 0, Setup, Teardown, prefix_setup_teardown_test_selector(Prefix, Setup, Teardown)).

parameterized_fixture(Module) ->
    fixture_tuple(Module, 1, fun default_test_selector/1).

parameterized_fixture(Module, Prefix) ->
    fixture_tuple(Module, 1, prefix_test_selector(Prefix)).

parameterized_fixture(Module, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    fixture_tuple(Module, 1, Setup, Teardown, setup_teardown_test_selector(Setup, Teardown)).

parameterized_fixture(Module, Prefix, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    fixture_tuple(Module, 1, Setup, Teardown, prefix_setup_teardown_test_selector(Prefix, Setup, Teardown)).

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

prefix_to_list(Prefix) when is_list(Prefix) -> Prefix;
prefix_to_list(Prefix) when is_atom(Prefix) -> atom_to_list(Prefix).

prefix_test_selector(Prefix) ->
    fun(F) ->
        default_test_selector(F) andalso lists:prefix(prefix_to_list(Prefix), atom_to_list(F))
    end.

setup_teardown_test_selector(Setup, Teardown) ->
    fun(F) ->
        default_test_selector(F) andalso F =/= Setup andalso F =/= Teardown
    end.

prefix_setup_teardown_test_selector(Prefix, Setup, Teardown) ->
    fun(F) ->
        default_test_selector(F) andalso F =/= Setup andalso F =/= Teardown andalso
            lists:prefix(prefix_to_list(Prefix), atom_to_list(F))
    end.

fixture_tuple(Module, Arity, Selector) ->
    fixture_tuple(Module, Arity, fun() -> ok end, fun(_) -> ok end, Selector).

fixture_tuple(Module, Arity, UserSetup, UserTeardown, Selector) when is_atom(UserSetup), is_atom(UserTeardown) ->
    fixture_tuple(Module, Arity, fun Module:UserSetup/0, fun Module:UserTeardown/1, Selector);
fixture_tuple(Module, Arity, UserSetup, UserTeardown, Selector) when is_function(UserSetup), is_function(UserTeardown) ->
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
