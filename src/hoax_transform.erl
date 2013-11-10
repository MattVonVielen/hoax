-module(hoax_transform).

-export([parse_transform/2]).
-include("hoax_int.hrl").

parse_transform(Forms, _Options) ->
    Res = [transform(Form) || Form <- Forms],
    io:format("~s\n\n", [forms_to_code(Res)]),
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

transform_call({call, _, {remote, Line, {atom, _, Mod}, {atom, _, Func}}, Args} = Call,
                Action) ->
    Rec = #expectation{
        key = make_key(Line, Mod, Func, length(Args)),
        desc = {string, Line, forms_to_code(Call)},
        line_num = {integer, Line, Line},
        args = transform_arguments(Line, Args),
        action = transform_action(Line, Action),
        call_count = {integer, Line, 0},
        expected_count = {atom, Line, undefined}
    },
    Fields = tl(tuple_to_list(Rec)),
    {tuple, Line, [{atom, Line, expectation} | Fields]}.

make_key(Line, Mod, Func, Arity) ->
    {tuple, Line, [
        {atom, Line, Mod},
        {atom, Line, Func},
        {integer, Line, Arity}
    ]}.

transform_arguments(Line, Args) ->
    list_to_forms(Line, [underscores_to_atoms(Arg) || Arg <- Args]).

underscores_to_atoms({var, Line, '_'}) ->
    {atom, Line, '_'};
underscores_to_atoms(Other) -> Other.

transform_action(Line, default) ->
    {atom, Line, default};
transform_action(_Line, Action = {'fun', _, _}) ->
    Action;
transform_action(Line, Action) ->
    {'fun', Line, {clauses, [{clause, Line, [], [], [Action]}]}}.

list_to_forms(Line, []) ->
    {nil, Line};
list_to_forms(Line, [H|T]) ->
    {cons, Line, H, list_to_forms(Line, T)}.

forms_to_code(Forms) when is_list(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms), [{paper, 128}, {ribbon, 128}]);
forms_to_code(Form) ->
    forms_to_code([Form]).