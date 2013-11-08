-module(hoax_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    Res = [transform(Form) || Form <- Forms],
    io:fwrite(erl_prettypr:format(erl_syntax:form_list(Res), [{paper, 100}, {ribbon, 100}])),
    io:fwrite("\n\n"),
    Res.

transform({function, Line, Name, Arity, Clauses}) ->
    {function, Line, Name, Arity, [transform_clause(Clause) || Clause <- Clauses]};
transform(Other) -> Other.

transform_clause({clause, Line, Head, Guard, Body}) ->
    Exprs = [transform_expression(Expr) || Expr <- Body],
    {clause, Line, Head, Guard, Exprs}.

transform_expression({call, Line, Call = {remote, _, {atom, _, hoax}, {atom, _, mock}},
                      Expectations}) ->
    Transformed = [transform_expectation(Expectation) || Expectation <- Expectations],
    {call, Line, Call, [list_to_forms(Line, Transformed)]};
transform_expression(Other) -> Other.

transform_expectation({op, _, '>', Call, Action}) ->
    transform_call(Call, Action);
transform_expectation(Call) ->
    transform_call(Call, default).

transform_call({call, _, {remote, Line, {atom, _, Mod}, {atom, _, Func}}, Args},
                Action) ->
    {tuple, Line, [
        {atom, Line, expectation},
        {integer, Line, Line},
        {atom, Line, Mod},
        {atom, Line, Func},
        list_to_forms(Line, [underscores_to_atoms(Arg) || Arg <- Args]),
        transform_action(Line, Action)
    ]}.

transform_action(Line, default) ->
    {atom, Line, default};
transform_action(_Line, Action = {'fun', _, _}) ->
    Action;
transform_action(Line, Action) ->
    {'fun', Line, {clauses, [{clause, Line, [], [], [Action]}]}}.

underscores_to_atoms({var, Line, '_'}) ->
    {atom, Line, '_'};
underscores_to_atoms(Other) -> Other.

list_to_forms(Line, []) ->
    {nil, Line};
list_to_forms(Line, [H|T]) ->
    {cons, Line, H, list_to_forms(Line, T)}.