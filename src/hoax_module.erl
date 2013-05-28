-module(hoax_module).

-export([compile/3]).

-include("hoax_int.hrl").

expectation(Mod, {Function, Args}) ->
    expectation(Mod, {Function, Args, default});
expectation(Mod, {Function, Args, Action}) ->
    #expectation{
        module   = Mod,
        function = Function,
        arity    = length(Args),
        args     = Args,
        action   = Action
    }.

compile(Mod, Funcs, Expectations) ->
    Exports = [ {Mod, Func} || Func <- Funcs ],
    Expects = [ expectation(Mod, Clause) || Clause <- Expectations ],
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

make_clauses_for_expect(Expect = #expectation{function = F, arity = A}, Dict) ->
    hoax_tab:init_expect(Expect),
    Clauses = clauses_for_func(Expect, Dict),
    Clause = hoax_syntax:exact_match_clause(Expect#expectation.args, [record_call(Expect), action_to_ast(Expect)]),
    dict:store({F, A}, [Clause|Clauses], Dict).

make_clause_for_export({Mod, Func = {F,A}}, Dict) ->
    case dict:find(Func, Dict) of
        {ok, _} -> Dict;
        error   ->
            Clause = unexpected_call_clause(unexpected_invocation, Mod, F, A),
            dict:store(Func, [Clause], Dict)
    end.

clauses_for_func(#expectation{module = M, function = F, arity = A}, Dict) ->
    case dict:find({F, A}, Dict) of
        error ->
            [unexpected_call_clause(unexpected_arguments, M, F, A)];
        {ok, Previous} ->
            Previous
    end.

record_call(#expectation{module = Mod, function = F, args = Args}) ->
    hoax_syntax:function_call(hoax_tab, record_call, [
            erl_syntax:atom(Mod), erl_syntax:atom(F), erl_syntax:abstract(Args)
        ]).

action_to_ast(#expectation{action = default}) ->
    erl_syntax:abstract('$_hoax_default_return_$');
action_to_ast(#expectation{action = {return, Value}}) ->
    erl_syntax:abstract(Value);
action_to_ast(#expectation{action = {Error, Value}}) ->
    hoax_syntax:raise(Error, erl_syntax:abstract(Value)).

unexpected_call_clause(Error, M, F, A) ->
    Args = hoax_syntax:variables(A),
    MFArgs = hoax_syntax:m_f_args(M, F, Args),
    Reason = erl_syntax:atom(Error),
    Exception = erl_syntax:tuple([Reason, MFArgs]),
    erl_syntax:clause(Args, [], [hoax_syntax:raise(error, Exception)]).
