-module(hoax_module).

-export([compile/3]).

transform_expectations(Mod, [{Function, Args} | Rest], Acc) ->
    E = {Mod, {Function, length(Args)}, Args, erl_syntax:abstract('$_hoax_default_return_$')},
    transform_expectations(Mod, Rest, [E|Acc]);
transform_expectations(Mod, [{Function, Args, {return, Value}} | Rest], Acc) ->
    E = {Mod, {Function, length(Args)}, Args, erl_syntax:abstract(Value)},
    transform_expectations(Mod, Rest, [E|Acc]);
transform_expectations(Mod, [{Function, Args, {throw, Error}} | Rest], Acc) ->
    E = {Mod, {Function, length(Args)}, Args, hoax_syntax:raise_error(erl_syntax:abstract(Error))},
    transform_expectations(Mod, Rest, [E|Acc]);
transform_expectations(_Mod, [], Acc) ->
    Acc.

compile(Mod, Funcs, Expectations) ->
    Exports = [ {Mod, Func} || Func <- Funcs ],
    Expects = transform_expectations(Mod, Expectations, []),
    Forms = erl_syntax:revert_forms([
                             hoax_syntax:module_attribute(Mod),
                             hoax_syntax:export_attribute(Funcs) |
                             make_functions(Exports, Expects)
                            ]),
    {ok, Mod, Bin} = compile:forms(Forms),
    hoax_tab:init_mod(Mod, code:is_sticky(Mod)),
    code:unstick_mod(Mod),
    code:load_binary(Mod, "", Bin).

make_functions(Exports, Expects) ->
    Dict0 = lists:foldl(fun make_clauses_for_expect/2, dict:new(),
                                   lists:reverse(Expects)),
    Dict = lists:foldl(fun make_clause_for_export/2, Dict0, Exports),
    dict:fold(fun make_function/3, [], Dict).

make_function({F,_}, Clauses, Functions) ->
    [ hoax_syntax:function(F, Clauses) | Functions ].

make_clauses_for_expect({Mod, {F,_} = Func, Args, Action}, Dict) ->
    hoax_tab:init_expect(Mod, F, Args),
    Clauses = clauses_for_func(Mod, Func, Dict),
    RecordCall = hoax_syntax:function_call(hoax_tab, record_call, [
            erl_syntax:atom(Mod), erl_syntax:atom(F), erl_syntax:abstract(Args)
        ]),
    Clause = hoax_syntax:exact_match_clause(Args, [RecordCall,Action]),
    dict:store(Func, [Clause|Clauses], Dict).

make_clause_for_export({Mod, Func = {F,A}}, Dict) ->
    case dict:find(Func, Dict) of
        {ok, _} -> Dict;
        error   ->
            Clause = unexpected_call_clause(unexpected_invocation, Mod, F, A),
            dict:store(Func, [Clause], Dict)
    end.

clauses_for_func(Mod, Func = {F, A}, Dict) ->
    case dict:find(Func, Dict) of
        error ->
            [unexpected_call_clause(unexpected_arguments, Mod, F, A)];
        {ok, Previous} ->
            Previous
    end.

unexpected_call_clause(Error, M, F, A) ->
    Args = hoax_syntax:variables(A),
    MFArgs = hoax_syntax:m_f_args(M, F, Args),
    Reason = erl_syntax:atom(Error),
    Exception = erl_syntax:tuple([Reason, MFArgs]),
    erl_syntax:clause(Args, [], [hoax_syntax:raise_error(Exception)]).
