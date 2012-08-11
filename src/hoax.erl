-module(hoax).

-export([stub/1, stub/2,
        %fake/2,
        stub_a/2, stub_a/3,
        expect/2,
        expect/3,
        and_return/1,
        and_throw/1,
        unload/0
    ]).

stub(M) -> stub(M, []).
stub(ModuleName, Expectations) ->
    ModName = list_to_atom(atom_to_list(ModuleName) ++ "_hoax"),
    Funcs = get_exports(ModuleName),
    compile(hoax_ast:module(ModName, Funcs, Expectations)).

stub_a(B, M) -> stub_a(B, M, []).
stub_a(Behaviour, ModuleName, Expectations) ->
    Funcs = Behaviour:behaviour_info(callbacks),
    compile(hoax_ast:module(ModuleName, Funcs, Expectations)).

expect(Func, Args) -> expect(Func, Args, and_return(ok)).
expect(Func, Args, Action) -> {Func, Args, Action}.

and_return(Value) -> {return, Value}.

and_throw(Error) -> {throw, Error}.

unload() -> ok.

%%%%%%%%%%%%%

compile(AST) ->
    Forms = erl_syntax:revert_forms(AST),
    {ok, Mod, Bin} = compile:forms(Forms),
    code:load_binary(Mod, "", Bin).

get_exports(ModuleName) ->
    [ E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info ].
