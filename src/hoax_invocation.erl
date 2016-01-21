-module(hoax_invocation).

-export([handle/3]).
-include("hoax_int.hrl").

handle(M, F, Args) ->
    case hoax_tab:lookup_expectations({M, F, length(Args)}) of
        [] ->
            erlang:error({unexpected_invocation, hoax_fmt:fmt({M, F, Args})});
        Records ->
            case find_matching_args(Args, Records) of
                false ->
                    unexpected_arguments(M, F, Args, Records);
                #expectation{call_count=X,expected_count=X,expected_args=ExpectedArgs} ->
                    erlang:error({too_many_invocations, X+1, hoax_fmt:fmt({M, F, ExpectedArgs})});
                #expectation{action = Action} = Record ->
                    hoax_tab:record_invocation(Record, Args),
                    perform(Action, Args)
            end
    end.

% As the most-common case, handle a single expectation separately to provide better error detail
unexpected_arguments(M, F, Args, [#expectation{expected_args = Expected}]) ->
    Arity = length(Args),
    FuncRep = flatfmt("~s:~s/~b", [M, F, Arity]),
    ArgsNotMatched = lists:foldl(
        fun ({_, ExpectedArg, ActualArg}, Acc) when ExpectedArg == ActualArg ->
                Acc;
            ({Seq, ExpectedArg, ActualArg}, Acc) ->
                Info = flatfmt("parameter ~b expected ~p but got ~p", [Seq, ExpectedArg, ActualArg]),
                [Info | Acc]
        end,
        [],
        lists:zip3(lists:seq(1, Arity), Expected, Args)
    ),
    erlang:error({unexpected_arguments, [FuncRep | lists:reverse(ArgsNotMatched)]});
unexpected_arguments(M, F, Args, Records) when length(Records) > 1 ->
    erlang:error({unexpected_arguments, hoax_fmt:fmt({M, F, Args})}).

find_matching_args(Args, Records) ->
    keyfind(Args, Records).

keyfind(ActualArgs, [ Expectation = #expectation{expected_args = ExpectedArgs} | Rest ]) ->
    case replace_wildcards(ActualArgs, ExpectedArgs) of
        ActualArgs -> Expectation;
        _          -> keyfind(ActualArgs, Rest)
    end;
keyfind(_, []) ->
    false.

perform(default, _)         -> '$_hoax_default_return_$';
perform({return_fun_result, Fun}, Args) ->
    erlang:apply(Fun, Args);
perform({return, Value}, _) -> Value;
perform({Error, Reason}, _) -> erlang:Error(Reason).

replace_wildcards(ActualArgs, ExpectedArgs) ->
    lists:zipwith(fun replace_wildcard/2, ActualArgs, ExpectedArgs).

replace_wildcard(Actual, '_') -> Actual;
replace_wildcard(_, Expected) -> Expected.

flatfmt(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
