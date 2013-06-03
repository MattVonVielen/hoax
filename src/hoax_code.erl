-module(hoax_code).

-export([
        get_export_list/2,
        get_callback_list/2,
        purge_and_delete/1
    ]).

-include("hoax_int.hrl").

get_export_list(ModuleName, ExpectedFunctions) ->
    case module_exists(ModuleName) of
        true ->
            [E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info];
        false ->
            [ {F, length(A)} || #expectation{key = {_,F,A}} <- ExpectedFunctions ]
    end.

get_callback_list(Behaviour, ModuleName) ->
    module_exists(ModuleName) andalso
        error({module_exists, ModuleName}),
    module_exists(Behaviour) orelse
        error({no_such_behaviour_to_mock, Behaviour}),
    erlang:function_exported(Behaviour, behaviour_info, 1) orelse
        error({not_a_behaviour, Behaviour}),
    Behaviour:behaviour_info(callbacks).

purge_and_delete(ModuleName) ->
    Sticky = code:is_sticky(ModuleName),
    code:purge(ModuleName),
    code:delete(ModuleName),
    code:ensure_loaded(ModuleName),
    restore_stickiness(ModuleName, Sticky).

module_exists(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {error, nofile} -> false;
        {module, ModuleName} -> true
    end.

restore_stickiness(ModuleName, true) ->
    code:stick_mod(ModuleName);
restore_stickiness(_, _) ->
    ok.
