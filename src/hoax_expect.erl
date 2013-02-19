-module(hoax_expect).

-export([
        make_expectation/3,
        return_value/1,
        throw_error/1,
        expand_expectations/3
    ]).

make_expectation(Func, Args, Action) ->
    {{Func,length(Args)}, Args, Action}.

return_value(Value) ->
    [erl_syntax:abstract(Value)].

throw_error(Error) ->
    [hoax_syntax:raise_error(erl_syntax:abstract(Error))].

expand_expectations(ModuleName, Funcs, Expectations) ->
    lists:foreach(
        fun({Func, _, _}) ->
            lists:member(Func, Funcs) orelse
                error({no_such_function_to_mock, Func})
        end, Expectations),

    [ {ModuleName, Func, Args, Action} ||
        {Func, Args, Action} <- Expectations ].
