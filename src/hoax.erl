-module(hoax).

-include("hoax_api.hrl").
-export(?HOAX_API).
-ignore_xref(?HOAX_API).

%% ===================================================================
%% hoax API
%% ===================================================================

start() ->
    hoax_srv:start().

stop() ->
    case erlang:whereis(hoax_srv) of
        undefined -> ok;
        _ ->
            lists:foreach(
              fun hoax_code:purge_and_delete/1,
              hoax_srv:stop())
    end.

mock(ModuleName, Expectations) ->
    Exports = hoax_code:get_function_list(ModuleName),
    Functions = hoax_expect:validate(Expectations),
    hoax_expect:assert_exported(Functions, Exports),
    hoax_module:compile(ModuleName, Exports, Expectations).

stub(Behaviour, ModuleName, Expectations) ->
    Callbacks = hoax_code:get_function_list(Behaviour, ModuleName),
    Functions = hoax_expect:validate(Expectations),
    hoax_expect:assert_exported(Functions, Callbacks),
    hoax_module:compile(ModuleName, Callbacks, Expectations).

fake(ModuleName, Expectations) ->
    Functions = hoax_expect:validate(Expectations),
    hoax_module:compile(ModuleName, Functions, Expectations).
