-module(hoax).

-export([start/0, stop/0]).
-export([mock/2, stub/3]).
-export([fixture/1, fixture/2, fixture/3, fixture/4, test/1]).
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
    {foreach, fun start/0, fun (_) -> stop() end, [
        {Module, F} || {F, 0} <- Module:module_info(exports),
        default_test_selector(F) == true
    ]}.

fixture(Module, Prefix) when is_atom(Prefix) ->
    fixture(Module, atom_to_list(Prefix));
fixture(Module, Prefix) when is_list(Prefix) ->
    {foreach, fun start/0, fun (_) -> stop() end, [
        {Module, F} || {F, 0} <- Module:module_info(exports),
            default_test_selector(F) == true,
            prefix_test_selector(Prefix, F)
    ]}.

fixture(Module, Setup, Teardown) when is_atom(Setup), is_atom(Teardown) ->
    {foreach,
        fun () -> start(), Module:Setup() end,
        fun (X) -> Module:Teardown(X), stop() end, [
        {Module, F} || {F, 0} <- Module:module_info(exports),
            default_test_selector(F) == true,
            setup_teardown_test_selector(F, Setup, Teardown)
    ]}.

fixture(Module, Prefix, Setup, Teardown) when is_atom(Prefix) ->
    fixture(Module, atom_to_list(Prefix), Setup, Teardown);
fixture(Module, Prefix, Setup, Teardown) when is_list(Prefix), is_atom(Setup), is_atom(Teardown) ->
    {foreach,
        fun () -> start(), Module:Setup() end,
        fun (X) -> Module:Teardown(X), stop() end, [
        {Module, F} || {F, 0} <- Module:module_info(exports),
            default_test_selector(F) == true,
            setup_teardown_test_selector(F, Setup, Teardown),
            prefix_test_selector(Prefix, F)
    ]}.

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

prefix_test_selector(Prefix, Func) ->
    Name = atom_to_list(Func),
    lists:prefix(Prefix, Name).

setup_teardown_test_selector(Func, Setup, Teardown) ->
    Func =/= Setup andalso Func =/= Teardown.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(MOD, module_for_testing_fixtures).

fixture_for_entire_module_without_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2,
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2,
        setup, test_function1, test_function2
    ],

    {foreach, _, _, Tests} = fixture(?MOD),

    ?assertEqual(ExpectedSortedFunctions, lists:sort([F || {_,F} <- Tests])).

fixture_for_entire_module_with_setup_and_teardown_test() ->
    ExpectedSortedFunctions = [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2,
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2,
        test_function1, test_function2
    ],

    {foreach, _, _, Tests} = fixture(?MOD, setup, teardown),

    ?assertEqual(ExpectedSortedFunctions, lists:sort([F || {_,F} <- Tests])).

fixture_for_prefix_without_setup_and_teardown_test() ->
    ExpectedSortedPrefix1Functions = [
        prefix1_setup, prefix1_test_function_1, prefix1_test_function_2
    ],
    ExpectedSortedPrefix2Functions = [
        prefix2_setup, prefix2_test_function_1, prefix2_test_function_2
    ],

    {foreach, _, _, Prefix1Tests} = fixture(?MOD, prefix1),
    {foreach, _, _, Prefix2Tests} = fixture(?MOD, prefix2),

    ?assertEqual(ExpectedSortedPrefix1Functions, lists:sort([F || {_,F} <- Prefix1Tests])),
    ?assertEqual(ExpectedSortedPrefix2Functions, lists:sort([F || {_,F} <- Prefix2Tests])),

    {foreach, _, _, Prefix1TestsFromString} = fixture(?MOD, "prefix1"),
    {foreach, _, _, Prefix2TestsFromString} = fixture(?MOD, "prefix2"),

    ?assertEqual(Prefix1Tests, Prefix1TestsFromString),
    ?assertEqual(Prefix2Tests, Prefix2TestsFromString).

fixture_for_prefix_with_setup_and_teardown_test() ->
    ExpectedSortedPrefix1Functions = [
        prefix1_test_function_1, prefix1_test_function_2
    ],
    ExpectedSortedPrefix2Functions = [
        prefix2_test_function_1, prefix2_test_function_2
    ],

    {foreach, _, _, Prefix1Tests} = fixture(?MOD, prefix1, prefix1_setup, prefix1_teardown),
    {foreach, _, _, Prefix2Tests} = fixture(?MOD, prefix2, prefix2_setup, prefix2_teardown),

    ?assertEqual(ExpectedSortedPrefix1Functions, lists:sort([F || {_,F} <- Prefix1Tests])),
    ?assertEqual(ExpectedSortedPrefix2Functions, lists:sort([F || {_,F} <- Prefix2Tests])),

    {foreach, _, _, Prefix1TestsFromString} = fixture(?MOD, "prefix1", prefix1_setup, prefix1_teardown),
    {foreach, _, _, Prefix2TestsFromString} = fixture(?MOD, "prefix2", prefix2_setup, prefix2_teardown),

    ?assertEqual(Prefix1Tests, Prefix1TestsFromString),
    ?assertEqual(Prefix2Tests, Prefix2TestsFromString).

-endif.