-module(hoax_dict).

-export([
        add_mod/1,
        del_mod/1,
        get_mods/0,
        has_mod/1
    ]).

add_mod(ModuleName) ->
    erlang:put(hoax_modules, [ModuleName|get_mods()]).

del_mod(ModuleName) ->
    erlang:put(hoax_modules, [M||M<-get_mods(),M/=ModuleName]).

get_mods() ->
    case erlang:get(hoax_modules) of
        undefined -> [];
        Other -> Other
    end.

has_mod(ModuleName) ->
    lists:member(ModuleName, get_mods()).
