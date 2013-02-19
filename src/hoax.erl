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

mock(ModuleName, Expectations) when is_list(Expectations) ->
    do_hoax(ModuleName, Expectations, strict);
mock(Behaviour, ModuleName) when is_atom(ModuleName) ->
    do_hoax(Behaviour, ModuleName, [], strict).

mock(Behaviour, ModuleName, Expectations) ->
    do_hoax(Behaviour, ModuleName, Expectations, strict).

stub(ModuleName, Expectations) when is_list(Expectations) ->
    do_hoax(ModuleName, Expectations, permissive);
stub(Behaviour, ModuleName) when is_atom(ModuleName) ->
    do_hoax(Behaviour, ModuleName, [], permissive).

stub(Behaviour, ModuleName, Expectations) ->
    do_hoax(Behaviour, ModuleName, Expectations, permissive).

fake(ModuleName, Expectations) ->
    Funcs = hoax_code:expectation_list_to_function_list(ModuleName, Expectations),
    make_hoax(ModuleName, Funcs, Expectations, strict).

expect(Func, Args) -> expect(Func, Args, and_return(ok)).
expect(Func, Args, Action) -> hoax_module:make_expectation(Func, Args, Action).

and_return(Value) -> hoax_module:return_value(Value).

and_throw(Error) -> hoax_module:throw_error(Error).

%%%%%%%%%%%%%

do_hoax(ModuleName, Expectations, Strict) ->
    Functions = hoax_code:get_function_list(ModuleName),
    make_hoax(ModuleName, Functions, Expectations, Strict).

do_hoax(Behaviour, ModuleName, Expectations, Strict) ->
    Callbacks = hoax_code:get_function_list(Behaviour, ModuleName),
    make_hoax(ModuleName, Callbacks, Expectations, Strict).

make_hoax(ModuleName, Funcs, Expectations, Strict) ->
    hoax_srv:add_mod(ModuleName),
    lists:foreach(
        fun({Func, _, _}) ->
            lists:member(Func, Funcs) orelse
                error({no_such_function_to_mock, Func})
        end, Expectations),

    Expects = [ {ModuleName, Func, Args, Action} ||
                {Func, Args, Action} <- Expectations ],
    hoax_module:compile(ModuleName, Funcs, Expects, Strict).
