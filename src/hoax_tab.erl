-module(hoax_tab).

-compile([export_all]).

-include_lib("stdlib/include/qlc.hrl").
-include("hoax_int.hrl").

insert(Expect) ->
    ets:insert(hoax, Expect).

create() ->
    ets:new(hoax, [named_table, public, bag, {keypos, #expectation.key}]).

delete() ->
    Mods = qlc:e(qlc:q([ M || #expectation{key = {M,_,_}} <- ets:table(hoax) ], [unique])),
    ets:delete(hoax),
    Mods.

unmet_expectations() ->
    qlc:e(qlc:q([ {M,F,A} || #expectation{key = {M,F,_}, args = A, call_count = 0} <- ets:table(hoax) ])).

increment_counter(E = #expectation{call_count=C}) ->
    ets:delete_object(hoax, E),
    ets:insert(hoax, E#expectation{call_count = C+1}).

lookup_expectations(Key) ->
    ets:lookup(hoax, Key).
