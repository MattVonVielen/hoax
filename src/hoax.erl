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
            lists:foreach(fun purge_and_delete/1, hoax_srv:stop())
    end.

stub(M) -> stub(M, []).
stub(ModuleName, Expectations) ->
    purge_and_delete(ModuleName),
    Funcs = case module_exists(ModuleName) of
        false ->
            erlang:error({no_such_module_to_stub, ModuleName});
        true ->
            get_exports(ModuleName)
        end,
    mock(ModuleName, Funcs, Expectations).

fake(ModuleName, Expectations) ->
    Funcs = case module_exists(ModuleName) of
        true ->
            erlang:error({module_exists, ModuleName});
        false ->
            [{F,length(A)} || {F,A,_} <- Expectations]
    end,
    mock(ModuleName, Funcs, Expectations).

%stub_a(B, M) -> stub_a(B, M, []).
%stub_a(Behaviour, ModuleName, Expectations) ->
%    Funcs = Behaviour:behaviour_info(callbacks),
%    mock(ModuleName, Funcs, Expectations).

expect(Func, Args) -> expect(Func, Args, and_return(ok)).
expect(Func, Args, Action) -> {Func, Args, Action}.

and_return(Value) -> {return, Value}.

and_throw(Error) -> {throw, Error}.

%%%%%%%%%%%%%

purge_and_delete(ModuleName) ->
    code:purge(ModuleName),
    code:delete(ModuleName).


mock(ModuleName, Funcs, Expectations) ->
    hoax_srv:add_mod(ModuleName),
    AST = hoax_ast:module(ModuleName, Funcs, Expectations),
    Forms = erl_syntax:revert_forms(AST),
    {ok, Mod, Bin} = compile:forms(Forms),
    code:load_binary(Mod, "", Bin).

module_exists(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {error, nofile} -> false;
        {module, ModuleName} -> true
    end.

get_exports(ModuleName) ->
    [E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info].
