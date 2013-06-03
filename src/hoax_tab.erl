-module(hoax_tab).

-compile([export_all]).

-include_lib("stdlib/include/qlc.hrl").
-include("hoax_int.hrl").

init_expect(Expect) ->
    ets:insert(hoax, Expect).

record_call(M,F,Args) ->
    ets:update_counter(hoax, {M,F,Args}, {#expectation.call_count, 1}).

create() ->
    ets:new(hoax, [named_table, public, {keypos, #expectation.key}]).

delete() ->
    Mods = qlc:e(qlc:q([ M || #expectation{key = {M,_,_}} <- ets:table(hoax) ], [unique])),
    ets:delete(hoax),
    Mods.

unmet_expectations() ->
    qlc:e(qlc:q([ Call || #expectation{key = Call, call_count = 0} <- ets:table(hoax) ])).
