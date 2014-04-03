-module(hoax_fun).

-export([
         find_name_in_module/2,
         create_wildcard_for_args/1
        ]).

find_name_in_module(Module, Fun) ->
    FunProps = erlang:fun_info(Fun),
    case lists:keyfind(module, 1, FunProps) of
        {module, Module} ->
            {name, FunName} = lists:keyfind(name, 1, FunProps),
            FunName;
        _AnotherModule ->
            not_exported
    end.

create_wildcard_for_args(Fun) when is_function(Fun) ->
    FunPL = erlang:fun_info(Fun),
    Arity = proplists:get_value(arity, FunPL),
    ['_' || _Val <- lists:seq(1, Arity)].
