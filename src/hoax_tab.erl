-module(hoax_tab).

-compile([export_all]).

-include_lib("stdlib/include/qlc.hrl").

init_expect(M,F,Args) ->
    ets:insert(hoax, {{calls,{M,F,Args}},0}).

record_call(M,F,Args) ->
    ets:update_counter(hoax, {calls,{M,F,Args}}, 1).

init_mod(Mod, IsSticky) ->
    ets:insert(hoax, { {mods, Mod}, IsSticky }).

create() ->
    ets:new(hoax, [named_table, public]).

delete() ->
    Mods = qlc:e(qlc:q([ {M,S} || {{mods,M},S} <- ets:table(hoax) ])),
    ets:delete(hoax),
    Mods.

unmet_expectations() ->
    qlc:e(qlc:q([ Call || {{calls,Call},0} <- ets:table(hoax) ])).
