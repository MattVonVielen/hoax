-module(hoax_module).

-export([module/4, make_expectation/3, return_value/1, throw_error/1]).

module(Mod, Funcs, Expects, Strict) ->
    Exports = [ {Mod, Func, Strict} || Func <- Funcs ],
    erl_syntax:revert_forms([
                             hoax_syntax:module_attribute(Mod),
                             hoax_syntax:export_attribute(Funcs) |
                             make_functions(Exports, Expects)
                            ]).

make_expectation(Func, Args, Action) -> {{Func,length(Args)}, Args, Action}.

return_value(Value) -> [erl_syntax:abstract(Value)].

throw_error(Error) -> [erlang_error(erl_syntax:abstract(Error))].

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

erlang_error(Error) ->
    hoax_syntax:function_call(erlang, error, [Error]).

unexpected_call_clause(Error, M, F, A) ->
    Args = hoax_syntax:variables(A),
    MFArgs = hoax_syntax:m_f_args(M, F, Args),
    Reason = erl_syntax:atom(Error),
    Exception = erl_syntax:tuple([Reason, MFArgs]),
    erl_syntax:clause(Args, [], [erlang_error(Exception)]).
