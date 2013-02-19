-module(hoax_syntax).

-export([
         module_attribute/1,
         export_attribute/1,
         attribute/2,
         function/2,
         exact_match_clause/2,
         function_call/3,
         variables/1,
         m_f_args/3,
         raise_error/1
     ]).

-type(func() :: {atom(), integer()}).
-type(attribute() :: erl_syntax:tree()).
-type(clause() :: erl_syntax:tree()).
-type(function_def() :: erl_syntax:tree()).
-type(function_call() :: erl_syntax:tree()).
-type(args() :: [erl_syntax:tree()]).

-spec(module_attribute( atom() ) -> attribute()).
module_attribute(Name) ->
    attribute(module, erl_syntax:atom(Name)).

-spec(export_attribute( [func()] ) -> attribute()).
export_attribute(Funcs) ->
    Exports = lists:map(fun arity_qualifier/1, Funcs),
    attribute(export, erl_syntax:list(Exports)).

-spec(attribute( atom(), erl_syntax:tree() ) -> attribute()).
attribute(Name, Value) ->
    erl_syntax:attribute(erl_syntax:atom(Name), [Value]).

-spec(function( atom(), [clause()] ) -> function_def()).
function(Name, Clauses) ->
    erl_syntax:function(erl_syntax:atom(Name), Clauses).

-spec(arity_qualifier( func() ) -> erl_syntax:tree()).
arity_qualifier({F, A}) ->
    erl_syntax:arity_qualifier(erl_syntax:atom(F), erl_syntax:integer(A)).

-spec(exact_match_clause( Args::list(), Body::[erl_syntax:tree()] ) -> clause()).
exact_match_clause(Args, Body) ->
    AbstractArgs = [erl_syntax:abstract(Arg) || Arg <- Args],
    erl_syntax:clause(AbstractArgs, [], Body).

-spec(function_call( atom(), atom(), args() ) -> function_call()).
function_call(Module, Function, Args) ->
    M = erl_syntax:atom(Module),
    F = erl_syntax:atom(Function),
    Q = erl_syntax:module_qualifier(M, F),
    erl_syntax:application(Q, Args).

-spec(variables( integer() ) -> args()).
variables(Arity) ->
    [ erl_syntax:variable([$V|integer_to_list(Num)]) || Num <- lists:seq(1,Arity) ].

-spec(m_f_args( atom(), atom(), args() ) -> erl_syntax:tree()).
m_f_args(Module, Function, Args) ->
    M = erl_syntax:atom(Module),
    F = erl_syntax:atom(Function),
    erl_syntax:tuple([M, F, erl_syntax:list(Args)]).

-spec(raise_error( term() ) -> erl_syntax:tree()).
raise_error(Error) ->
    function_call(erlang, error, [Error]).
