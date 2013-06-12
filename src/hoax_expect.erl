-module(hoax_expect).

-export([
        assert_exported/2,
        parse/2
    ]).

-include("hoax_int.hrl").

assert_exported([Expect = #expectation{key={_,F,A}} | Rest], Exports) ->
    lists:member({F, A}, Exports) orelse
        error({no_such_function_to_mock, {F, A}}),
    hoax_tab:insert(Expect),
    assert_exported(Rest, Exports);
assert_exported([], _) ->
    ok.

parse(Mod,[]) ->
    error({no_expectations_for_mock, Mod});
parse(Mod,Expects) ->
    [expectation(Mod, Ex) || Ex <- Expects].

expectation(Mod, {Function, Args}) when is_atom(Mod), is_list(Args) ->
    expectation(Mod, Function, Args, default, undefined);
expectation(Mod, {Function, Args, Count}) when is_atom(Mod),
                                               is_list(Args),
                                               is_integer(Count) ->
    expectation(Mod, Function, Args, default, Count);
expectation(Mod, {Function, Args, {X,Y}}) when is_atom(Mod),
                                               is_list(Args),
                                               X == return;
                                               X == error;
                                               X == exit;
                                               X == throw ->
    expectation(Mod, Function, Args, {X,Y}, undefined);
expectation(Mod, {Function, Args, {X,Y}, Count}) when is_atom(Mod),
                                                      is_list(Args),
                                                      is_integer(Count),
                                                      X == return;
                                                      X == error;
                                                      X == exit;
                                                      X == throw ->
    expectation(Mod, Function, Args, {X,Y}, Count);
expectation(_, Other) ->
    error({bad_expectation_syntax, Other}).

expectation(Mod, Function, Args, Action, Count) ->
    #expectation{
        key = {Mod, Function, length(Args)},
        args = Args,
        action = Action,
        expected_count = Count
    }.
