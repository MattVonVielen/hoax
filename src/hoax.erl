-module(hoax).

-export([start/0, stop/0]).
-export([mock/2, stub/3]).
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
