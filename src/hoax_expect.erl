-module(hoax_expect).

-export([
        validate/1,
        assert_exported/2
    ]).

validate(Expectations) ->
    validate(Expectations, []).

validate([{Function, Args} | Rest], Acc)
    when is_list(Args) ->
    validate(Rest, [{Function, length(Args)} | Acc]);
validate([{Function, Args, Action} = Expectation | Rest], Acc)
    when is_list(Args) ->
    case valid_action(Action) of
        true ->
            validate(Rest, [{Function, length(Args)} | Acc]);
        false ->
            error({bad_expectation_syntax, Expectation})
    end;
validate([Other | _Rest], _Acc) ->
    error({bad_expectation_syntax, Other});
validate([], Acc) ->
    Acc.

assert_exported([Function | Rest], Exports) ->
    lists:member(Function, Exports) orelse
        error({no_such_function_to_mock, Function}),
    assert_exported(Rest, Exports);
assert_exported([], _) ->
    ok.

valid_action({return, _}) -> true;
valid_action({error, _}) -> true;
valid_action({exit, _}) -> true;
valid_action({throw, _}) -> true;
valid_action(_) -> false.
