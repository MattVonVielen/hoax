-module(hoax_tab).

-export([create/0, delete/0, insert/1, lookup/1, increment_counter/1, unmet_expectations/0]).

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
    NoExpectedCountGiven = qlc:q([
                X || X = #expectation{call_count = 0, expected_count = undefined}
                     <- ets:table(hoax) ]),

    FewerCallsThanExpected = qlc:q([
                X || X = #expectation{call_count = C, expected_count = E}
                     <- ets:table(hoax), is_integer(E), C < E ]),

    qlc:e(qlc:append(NoExpectedCountGiven, FewerCallsThanExpected)).

increment_counter(E = #expectation{call_count=C}) ->
    ets:delete_object(hoax, E),
    ets:insert(hoax, E#expectation{call_count = C+1}).

lookup(Key) ->
    ets:lookup(hoax, Key).
