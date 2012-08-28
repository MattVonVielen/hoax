-module(hoax_code).

-export([
        compile/1,
        module_exists/1,
        get_exports/1,
        purge_and_delete/1
    ]).

compile(Forms) ->
    {ok, Mod, Bin} = compile:forms(Forms),
    code:load_binary(Mod, "", Bin).

module_exists(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {error, nofile} -> false;
        {module, ModuleName} -> true
    end.

get_exports(ModuleName) ->
    [E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info].

purge_and_delete(ModuleName) ->
    code:purge(ModuleName),
    code:delete(ModuleName).
