-module(hoax_fun).

-export([
         create_wildcard_for_args/1
        ]).

create_wildcard_for_args(Fun) when is_function(Fun) ->
    FunPL = erlang:fun_info(Fun),
    Arity = proplists:get_value(arity, FunPL),
    ['_' || _Val <- lists:seq(1, Arity)].
