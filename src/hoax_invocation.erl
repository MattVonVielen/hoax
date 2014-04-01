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
                    erlang:error({unexpected_arguments, hoax_fmt:fmt({M, F, Args})});
                #expectation{call_count=X,expected_count=X,args=ExpectedArgs} ->
                    erlang:error({too_many_invocations, X+1, hoax_fmt:fmt({M, F, ExpectedArgs})});
                #expectation{action = Action} = Record ->
                    hoax_tab:increment_counter(Record),
                    perform(Action, Args)
            end
    end.

find_matching_args(Args, Records) ->
    keyfind(Args, Records).

keyfind(ActualArgs, [ Expectation = #expectation{args = ExpectedArgs} | Rest ]) ->
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
