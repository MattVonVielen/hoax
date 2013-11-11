-module(hoax_transform).

-export([parse_transform/2, format_error/1]).
-include("hoax_int.hrl").

parse_transform(Forms, _Options) ->
    Res = forms(Forms),
%%     io:format("~s\n\n", [forms_to_code(Res)]),
    Res.

forms([F0|Fs0]) ->
    F1 = try transform(F0)
         catch throw:{Line, Error} ->
             {error, {Line, ?MODULE, Error}}
         end,
    Fs1 = forms(Fs0),
    [F1|Fs1];
forms([]) -> [].

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
    transform_expectation(Call, Action);
transform_expectation(Call) ->
    transform_expectation(Call, default).

transform_expectation({call, _, {remote, Line, {atom, _, Mod}, {atom, _, Func}}, Args} = Call, Action) ->
    transform_call(Line, Mod, Func, Args, Call, Action, undefined);
transform_expectation({call, _, {atom, _, CountWord}, [{call, _, {remote, Line, {atom, _, Mod}, {atom, _, Func}}, Args} = Call]},
                      Action) when CountWord == never; CountWord == once; CountWord == twice ->
    Count = case CountWord of never -> 0; once -> 1; twice -> 2 end,
    transform_call(Line, Mod, Func, Args, Call, Action, Count);
transform_expectation({call, _, {atom, _, times}, [{integer, _, Count}, {call, _, {remote, Line, {atom, _, Mod}, {atom, _, Func}}, Args} = Call]},
                      Action) ->
    transform_call(Line, Mod, Func, Args, Call, Action, Count);
transform_expectation(Other, _) ->
    throw({(element(2, Other)), ["bad hoax expectation: ", forms_to_code(Other)]}).

transform_call(Line, Mod, Func, Args, Call, Action, ExpectedCount) ->
    Rec = #expectation{
        key = make_key(Line, Mod, Func, length(Args)),
        desc = {string, Line, forms_to_code(Call)},
        line_num = {integer, Line, Line},
        args = transform_arguments(Line, Args),
        action = transform_action(Line, Action),
        call_count = {integer, Line, 0},
        expected_count = transform_expected_count(Line, ExpectedCount)
    },
    Fields = tl(tuple_to_list(Rec)),
    {tuple, Line, [{atom, Line, expectation} | Fields]}.

transform_expected_count(Line, undefined) ->
    {atom, Line, undefined};
transform_expected_count(Line, Count) ->
    {integer, Line, Count}.

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
transform_action(_Line, Action = {'fun', _, {clauses, [{clause, _, [], _, _} | _]}}) ->
    Action;
transform_action(_Line, {'fun', Line, _}) ->
    throw({Line, "bad hoax expectation: functions used for return values must be nullary"});
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

%% This function is called by the Erlang compiler to obtain an error
%% message which will be shown to the user.
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _ -> io_lib:write(Message)
    end.
