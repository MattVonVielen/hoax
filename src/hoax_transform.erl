-module(hoax_transform).

-export([parse_transform/2, format_error/1]).
-include("hoax_int.hrl").

parse_transform(Forms, _Options) ->
    transform_forms(Forms, []).

transform_forms([], Acc) -> lists:reverse(Acc);
transform_forms([Form | Rest], Acc) ->
    Transformed = try transform_form(Form)
                  catch throw:{Line, Error} ->
                      {error, {Line, ?MODULE, Error}}
                  end,
    transform_forms(Rest, [Transformed | Acc]).

transform_form({function, Line, Name, Arity, Clauses}) ->
    {function, Line, Name, Arity, [transform_expression(Clause) || Clause <- Clauses]};
transform_form(Other) ->
    Other.

transform_expression({clause, Line, Head, Guard, Body}) ->
    {clause, Line, Head, Guard, [transform_expression(Expr) || Expr <- Body]};
% recurse through fun-expressions
transform_expression({'fun', Line, {clauses, Clauses}}) ->
    {'fun', Line, {clauses, [transform_expression(Clause) || Clause <- Clauses]}};
% recurse through try-expressions
transform_expression({'try', Line, Body, Clauses, Handlers, After}) ->
    {'try', Line, [transform_expression(Expr) || Expr <- Body],
                  [transform_expression(Clause) || Clause <- Clauses],
                  [transform_expression(Handler) || Handler <- Handlers],
                  [transform_expression(Expr) || Expr <- After]};
% we've found a call to either expect/* or allow/*
transform_expression({call, Line, Call = {remote, Line, {atom, Line, hoax}, {atom, Line, Verb}}, Expectations}) when Verb == expect; Verb == allow ->
    Contents = lists:flatten([transform_expectation_contents(Expectation) || Expectation <- Expectations]),
    {call, Line, Call, [list_to_forms(Line, Contents)]};
transform_expression({call, Line, Call, Arguments}) ->
    {call, Line, Call, [transform_expression(Arg) || Arg <- Arguments]};
transform_expression(Other) ->
    Other.

transform_expectation_contents({'receive', _, Clauses}) ->
    [create_expectation_record(Call, {Guard, Action}) || {clause, _, [Call], Guard, Action} <- Clauses];
transform_expectation_contents({call, _, _, _} = Call) ->
    create_expectation_record(Call, default);
transform_expectation_contents(Other) ->
    throw({(element(2, Other)), ["bad hoax expectation: ", forms_to_code(Other)]}).

create_expectation_record({call, _, {remote, Line, {atom, _, Mod}, {atom, _, Func}}, Args}, Action) ->
    Rec = #expectation{
        key = {Mod, Func, length(Args)},
        expected_args = Args,
        action = {Action, Args}
    },
    FieldProplist = lists:zip(record_info(fields, expectation), tl(tuple_to_list(Rec))),
    Fields = [field_to_forms(Name, Value, Line) || {Name, Value} <- FieldProplist],
    {tuple, Line, [{atom, Line, expectation} | Fields]};
create_expectation_record(Other, _) ->
    throw({(element(2, Other)), ["bad hoax expectation: ", forms_to_code(Other)]}).

field_to_forms(key, {Mod, Func, Arity}, Line) ->
    {tuple, Line, [{atom, Line, Mod}, {atom, Line, Func}, {integer, Line, Arity}]};
field_to_forms(desc, Desc, Line) ->
    {string, Line, Desc};
field_to_forms(line_num, Line, Line) ->
    {integer, Line, Line};
field_to_forms(expected_args, Args, Line) ->
    list_to_forms(Line, [underscores_to_atoms(Arg) || Arg <- Args]);
field_to_forms(actual_args, Args, Line) ->
    list_to_forms(Line, Args);
field_to_forms(action, {default, _}, Line) ->
    {atom, Line, default};
field_to_forms(action, {{Guard, Action}, Args}, Line) ->
    {'fun', Line, {clauses, [{clause, Line, wildcard_variables(Line, length(Args)), Guard, Action}]}};
field_to_forms(call_count, Count, Line) ->
    {integer, Line, Count};
field_to_forms(expected_count, undefined, Line) ->
    {atom, Line, undefined};
field_to_forms(expected_count, Count, Line) ->
    {integer, Line, Count}.

underscores_to_atoms({var, Line, '_'}) ->
    {atom, Line, '_'};
underscores_to_atoms(Other) ->
    Other.

wildcard_variables(Line, Arity) ->
    [ {var, Line, list_to_atom([$_|integer_to_list(Num)])} || Num <- lists:seq(1, Arity)].

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
