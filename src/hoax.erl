-module(hoax).

-include("hoax_api.hrl").
-export([start/0, stop/0]).
-export(?HOAX_API).
-ignore_xref(?HOAX_API).

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
    Functions = hoax_expect:validate(Expectations),
    Exports = hoax_code:get_export_list(ModuleName, Functions),
    hoax_expect:assert_exported(Functions, Exports),
    hoax_module:compile(ModuleName, Functions, Expectations).

stub(Behaviour, ModuleName, Expectations) ->
    Callbacks = hoax_code:get_callback_list(Behaviour, ModuleName),
    Functions = hoax_expect:validate(Expectations),
    hoax_expect:assert_exported(Functions, Callbacks),
    hoax_module:compile(ModuleName, Callbacks, Expectations).
