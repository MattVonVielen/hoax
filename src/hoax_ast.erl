-module(hoax_ast).

-compile([export_all]).

-import(erl_syntax, [
        abstract/1,
        application/2,
        arity_qualifier/2,
        atom/1,
        attribute/2,
        clause/3,
        function/2,
        integer/1,
        list/1,
        module_qualifier/2,
        tuple/1,
        underscore/0,
        variable/1
    ]).

% -type(ast() :: erl_syntax:syntax_tree()).
% -type(func() :: {FA::{atom(), integer()}, {Export::ast(), Clauses::[ast()]}}).

module(Name, Funcs, Expectations) ->
    FinalClauses = lists:foldl(
        fun add_expectation/2,
        default_clauses(Name, Funcs, fun unexpected_invocation/2),
        Expectations),

    Exports = [make_export(K) || K <- dict:fetch_keys(FinalClauses)],

    [ module_attr(Name), export_attr(Exports) ] ++
    dict:fold(fun({F,_}, Clauses, Acc) ->
                [function(atom(F), Clauses)|Acc]
        end, [], FinalClauses).

default_clauses(Name, Funcs, DefaultClauseFun) ->
    lists:foldl(
        fun(F,Acc) ->
            dict:store(F, [DefaultClauseFun(Name, F)], Acc)
        end, dict:new(), Funcs).

module_attr(Name) ->
    attribute(atom(module), [atom(Name)]).

export_attr(Funcs) ->
    attribute(atom(export), [list(Funcs)]).

make_export({F, A}) ->
    arity_qualifier(atom(F), integer(A)).

unexpected_invocation(_, {_, A}) ->
    Args = lists:map(fun(_) -> underscore() end, lists:seq(1,A)),
    clause(Args, [], [atom(ok)]).

unexpected_invocation_exception(M, {F, A}) ->
    Args = variables_for_arity(A),
    Exception = tuple([atom(unexpected_invocation), m_f_args(M, F, Args)]),
    clause(Args, [], [throw_exception(Exception)]).

add_expectation({Func, Args, Action}, FuncDict) ->
    Key = {Func, length(Args)},
    case dict:find(Key, FuncDict) of
        error ->
            erlang:throw({no_such_function, Key});
        {ok, Clauses} ->
            Clause = expected_invocation(Args, Action),
            dict:store(Key, [Clause|Clauses], FuncDict)
    end.

expected_invocation(Args, Body) ->
    clause([abstract(Arg) || Arg <- Args], [], [function_body(Body)]).

function_body({return, Value}) ->
    abstract(Value);
function_body({throw, Error}) ->
    throw_exception(abstract(Error)).

variables_for_arity(Arity) ->
    [ variable("V"++integer_to_list(Num)) || Num <- lists:seq(1,Arity) ].

throw_exception(Exception) ->
    Throw = module_qualifier(atom(erlang), atom(throw)),
    application(Throw, [Exception]).

m_f_args(M, F, Args) ->
    tuple([atom(M), atom(F), list(Args)]).

