-module(hoax_ast).

-export([module/4]).

-type(return_action() :: {return, term()}).
-type(throw_action() :: {throw, term()}).
-type(action() :: return_action() | throw_action()).
-type(expectation() :: {atom(), [term()], action()}).

module(Mod, Funcs, Expects, Strict) ->
    erl_syntax:revert_forms([
                             hoax_syntax:module_attribute(Mod),
                             hoax_syntax:export_attribute(Funcs) |
                             make_functions(Mod, Funcs, Expects, Strict)
                            ]).

make_functions(Mod, Funcs, Expects, Strict) ->
    Defaults = [ {Mod, Func, Strict} || Func <- Funcs ],
    ClauseDict = make_clauses(Defaults, Expects),
    dict:fold(fun make_function/3, [], ClauseDict).

make_function({F,_}, Clauses, Functions) ->
    [ hoax_syntax:function(F, Clauses) | Functions ].

make_clauses(Defaults, Expects) ->
    ClauseDict = lists:foldl(fun make_default_clause/2, dict:new(), Defaults),
    lists:foldl(fun add_expectation/2, ClauseDict, Expects).

make_default_clause({Mod, Func = {F, A}, Strict}, Dict) ->
    Clause = case Strict of
        permissive ->
            hoax_syntax:wildcard_clause(A, [erl_syntax:atom(ok)]);
        strict ->
            unexpected_invocation_clause(Mod, F, A)
    end,
    dict:store(Func, [Clause], Dict).

-spec(add_expectation( expectation(), dict() ) -> dict()).
add_expectation({Func, Args, Action}, FuncDict) ->
    Key = {Func, length(Args)},
    case dict:find(Key, FuncDict) of
        error ->
            error({no_such_function_to_stub, Key});
        {ok, Clauses} ->
            Body = case Action of
                {return, Value} -> erl_syntax:abstract(Value);
                {throw, Error} -> erlang_error(Error)
            end,
            Clause = hoax_syntax:exact_match_clause(Args, [Body]),
            dict:store(Key, [Clause|Clauses], FuncDict)
    end.

erlang_error(Error) ->
    hoax_syntax:function_call(erlang, error, [erl_syntax:abstract(Error)]).

unexpected_invocation_clause(M, F, A) ->
    Args = hoax_syntax:variables(A),
    MFArgs = hoax_syntax:m_f_args(M, F, Args),
    Reason = erl_syntax:atom(unexpected_invocation),
    Exception = erl_syntax:tuple([Reason, MFArgs]),
    erl_syntax:clause(Args, [], [erlang_error(Exception)]).
