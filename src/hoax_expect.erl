-module(hoax_expect).

-export([
        validate/1,
        assert_exported/2
    ]).

validate(Expectations) ->
    validate(Expectations, []).

validate([{Function, Args} | Rest], Acc) when is_list(Args) ->
    validate(Rest, [{Function, length(Args)} | Acc]);
validate([{Function, Args, {return, _Value}} | Rest], Acc) when is_list(Args) ->
    validate(Rest, [{Function, length(Args)} | Acc]);
validate([{Function, Args, {throw, _Error}} | Rest], Acc) when is_list(Args) ->
    validate(Rest, [{Function, length(Args)} | Acc]);
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
