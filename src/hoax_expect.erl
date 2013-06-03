-module(hoax_expect).

-export([
        assert_exported/2,
        parse/2
    ]).

-include("hoax_int.hrl").

assert_exported([Expect = #expectation{key = {_, F, A}} | Rest], Exports) ->
    lists:member({F, length(A)}, Exports) orelse
        error({no_such_function_to_mock, {F, length(A)}}),
    hoax_tab:init_expect(Expect),
    assert_exported(Rest, Exports);
assert_exported([], _) ->
    ok.

parse(Mod,[]) ->
    error({no_expectations_for_mock, Mod});
parse(Mod,Expects) ->
    [expectation(Mod, Ex) || Ex <- Expects].

expectation(Mod, {Function, Args}) when is_atom(Mod), is_list(Args) ->
    expectation(Mod, Function, Args, default);
expectation(Mod, {Function, Args, {X,Y}}) when is_atom(Mod),
                                               is_list(Args),
                                               X == return;
                                               X == error;
                                               X == exit;
                                               X == throw ->
    expectation(Mod, Function, Args, {X,Y});
expectation(_, Other) ->
    error({bad_expectation_syntax, Other}).

expectation(Mod, Function, Args, Action) ->
    #expectation{
        key    = {Mod, Function, Args},
        action = Action
    }.
