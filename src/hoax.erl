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
    Functions = hoax_code:get_function_list(ModuleName),
    make_hoax(ModuleName, Functions, Expectations, strict).

mock(Behaviour, ModuleName, Expectations) ->
    Callbacks = hoax_code:get_function_list(Behaviour, ModuleName),
    make_hoax(ModuleName, Callbacks, Expectations, strict).

stub(ModuleName, Expectations) ->
    Functions = hoax_code:get_function_list(ModuleName),
    make_hoax(ModuleName, Functions, Expectations, permissive).

stub(Behaviour, ModuleName, Expectations) ->
    Callbacks = hoax_code:get_function_list(Behaviour, ModuleName),
    make_hoax(ModuleName, Callbacks, Expectations, permissive).

fake(ModuleName, Expectations) ->
    Funcs = hoax_code:expectation_list_to_function_list(ModuleName, Expectations),
    make_hoax(ModuleName, Funcs, Expectations, strict).

expect(Func, Args) -> expect(Func, Args, and_return(ok)).
expect(Func, Args, Action) -> hoax_expect:make_expectation(Func, Args, Action).

and_return(Value) -> hoax_expect:return_value(Value).

and_throw(Error) -> hoax_expect:throw_error(Error).

%%%%%%%%%%%%%

make_hoax(ModuleName, Funcs, Expectations, Strict) ->
    hoax_srv:add_mod(ModuleName),
    Expanded = hoax_expect:expand_expectations(ModuleName, Funcs, Expectations),
    hoax_module:compile(ModuleName, Funcs, Expanded, Strict).
