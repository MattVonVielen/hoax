-module(hoax_tab).

-compile([export_all]).

-include_lib("stdlib/include/qlc.hrl").
-include("hoax_int.hrl").

insert(Expect) ->
    ets:insert(hoax, Expect).

create() ->
    ets:new(hoax, [named_table, public, {keypos, #expectation.key}]).

delete() ->
    Mods = qlc:e(qlc:q([ M || #expectation{key = {M,_,_}} <- ets:table(hoax) ], [unique])),
    ets:delete(hoax),
    Mods.

unmet_expectations() ->
    qlc:e(qlc:q([ Call || #expectation{key = Call, call_count = 0} <- ets:table(hoax) ])).

increment_counter(Key) ->
    ets:update_counter(hoax, Key, {#expectation.call_count, 1}).

lookup_action(Key) ->
    case ets:lookup(hoax, Key) of
        []    ->
            {error, {unexpected_invocation, Key}};
        [Rec] ->
            increment_counter(Key),
            Rec#expectation.action
    end.
