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
    hoax_code:module_exists(ModuleName) andalso
        error({module_exists, ModuleName}),
    Funcs = [ element(1,X) || X <- Expectations ],
    make_hoax(ModuleName, Funcs, Expectations, strict).

expect(Func, Args) -> expect(Func, Args, and_return(ok)).
expect(Func, Args, Action) -> hoax_ast:make_expectation(Func, Args, Action).

and_return(Value) -> hoax_ast:return_value(Value).

and_throw(Error) -> hoax_ast:throw_error(Error).

%%%%%%%%%%%%%

do_hoax(ModuleName, Expectations, Strict) ->
    hoax_code:module_exists(ModuleName) orelse
        error({no_such_module_to_mock, ModuleName}),
    make_hoax(ModuleName, hoax_code:get_exports(ModuleName), Expectations,
              Strict).

do_hoax(Behaviour, ModuleName, Expectations, Strict) ->
    hoax_code:module_exists(Behaviour) orelse
        error({no_such_behaviour_to_mock, Behaviour}),
    erlang:function_exported(Behaviour, behaviour_info, 1) orelse
        error({not_a_behaviour, Behaviour}),
    hoax_code:module_exists(ModuleName) andalso
        error({module_exists, ModuleName}),
    make_hoax(ModuleName, hoax_code:get_callbacks(Behaviour), Expectations,
              Strict).

make_hoax(ModuleName, Funcs, Expectations, Strict) ->
    hoax_srv:add_mod(ModuleName),
    lists:foreach(
        fun({Func, _, _}) ->
            lists:member(Func, Funcs) orelse
                error({no_such_function_to_mock, Func})
        end, Expectations),

    Expects = [ {ModuleName, Func, Args, Action} ||
                {Func, Args, Action} <- Expectations ],
    Forms = hoax_ast:module(ModuleName, Funcs, Expects, Strict),
    hoax_code:compile(Forms).
