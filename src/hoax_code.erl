-module(hoax_code).

-export([
        module_exists/1,
        get_exports/1,
        get_callbacks/1,
        purge_and_delete/1
    ]).

module_exists(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {error, nofile} -> false;
        {module, ModuleName} -> true
    end.

get_exports(ModuleName) ->
    [E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info].

get_callbacks(Behaviour) ->
    case module_exists(Behaviour) of
        false ->
            {error, {no_such_behaviour_to_mock, Behaviour}};
        true ->
            case erlang:function_exported(Behaviour, behaviour_info, 1) of
                false ->
                    {error, {not_a_behaviour, Behaviour}};
                true ->
                    {ok, Behaviour:behaviour_info(callbacks)}
            end
    end.

purge_and_delete(ModuleName) ->
    code:purge(ModuleName),
    code:delete(ModuleName).
