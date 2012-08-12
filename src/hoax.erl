-module(hoax).

-export([stub/1, stub/2,
        fake/2,
        stub_a/2, stub_a/3,
        expect/2,
        expect/3,
        and_return/1,
        and_throw/1,
        unload/1
    ]).

stub(M) -> stub(M, []).
stub(ModuleName, Expectations) ->
    ModName = list_to_atom(atom_to_list(ModuleName) ++ "_hoax"),
    Funcs = get_exports(ModuleName),
    mock(ModName, Funcs, Expectations).

fake(ModuleName, Expectations) ->
    case module_exists(ModuleName) of
        true ->
            erlang:error({module_exists, ModuleName});
        false ->
            Funcs = [{F,length(A)} || {F,A,_} <- Expectations],
            mock(ModuleName, Funcs, Expectations)
    end.

stub_a(B, M) -> stub_a(B, M, []).
stub_a(Behaviour, ModuleName, Expectations) ->
    Funcs = Behaviour:behaviour_info(callbacks),
    mock(ModuleName, Funcs, Expectations).

expect(Func, Args) -> expect(Func, Args, and_return(ok)).
expect(Func, Args, Action) -> {Func, Args, Action}.

and_return(Value) -> {return, Value}.

and_throw(Error) -> {throw, Error}.

unload(ModuleName) ->
    code:purge(ModuleName),
    code:delete(ModuleName).

%%%%%%%%%%%%%

mock(ModuleName, Funcs, Expectations) ->
    AST = hoax_ast:module(ModuleName, Funcs, Expectations),
    Forms = erl_syntax:revert_forms(AST),
    {ok, Mod, Bin} = compile:forms(Forms),
    code:load_binary(Mod, "", Bin).

get_exports(ModuleName) ->
    case module_exists(ModuleName) of
        false ->
            erlang:error({no_such_module_to_stub, ModuleName});
        true ->
            [ E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info ]
    end.

module_exists(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {error, nofile} -> false;
        {module, ModuleName} -> true
    end.

