-module(hoax_module).

-export([compile/4]).

compile(Mod, Funcs, Expects, Strict) ->
    Exports = [ {Mod, Func, Strict} || Func <- Funcs ],
    Forms = erl_syntax:revert_forms([
                             hoax_syntax:module_attribute(Mod),
                             hoax_syntax:export_attribute(Funcs) |
                             make_functions(Exports, Expects)
                            ]),
    {ok, Mod, Bin} = compile:forms(Forms),
    code:load_binary(Mod, "", Bin),
    hoax_srv:add_mod(Mod).

make_functions(Exports, Expects) ->
    Dict0 = lists:foldl(fun make_clauses_for_expect/2, dict:new(),
                                   lists:reverse(Expects)),
    Dict = lists:foldl(fun make_clause_for_export/2, Dict0, Exports),
    dict:fold(fun make_function/3, [], Dict).

make_function({F,_}, Clauses, Functions) ->
    [ hoax_syntax:function(F, Clauses) | Functions ].

make_clauses_for_expect({Mod, Func, Args, Action}, Dict) ->
    Clauses = clauses_for_func(Mod, Func, Dict),
    Clause = hoax_syntax:exact_match_clause(Args, Action),
    dict:store(Func, [Clause|Clauses], Dict).

make_clause_for_export(Export = {_, Func, _}, Dict) ->
    case dict:find(Func, Dict) of
        {ok, _} -> Dict;
        error   -> make_unexpected_call_clause(Export, Dict)
    end.

clauses_for_func(Mod, Func = {F, A}, Dict) ->
    case dict:find(Func, Dict) of
        error ->
            [unexpected_call_clause(unexpected_arguments, Mod, F, A)];
        {ok, Previous} ->
            Previous
    end.

make_unexpected_call_clause({_, Func = {_, A}, permissive}, Dict) ->
    Clause = hoax_syntax:wildcard_clause(A, [erl_syntax:atom(ok)]),
    dict:store(Func, [Clause], Dict);
make_unexpected_call_clause({Mod, Func = {F, A}, strict}, Dict) ->
    Clause = unexpected_call_clause(unexpected_invocation, Mod, F, A),
    dict:store(Func, [Clause], Dict).

unexpected_call_clause(Error, M, F, A) ->
    Args = hoax_syntax:variables(A),
    MFArgs = hoax_syntax:m_f_args(M, F, Args),
    Reason = erl_syntax:atom(Error),
    Exception = erl_syntax:tuple([Reason, MFArgs]),
    erl_syntax:clause(Args, [], [hoax_syntax:raise_error(Exception)]).
