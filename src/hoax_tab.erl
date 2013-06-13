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
    qlc:e(qlc:q([ format_unmet_expectation(X) || X = #expectation{
                        call_count = C, expected_count = E} <- ets:table(hoax),
                                                 (is_integer(E) andalso
                                                  C < E) orelse C == 0 ])).

increment_counter(E = #expectation{call_count=C}) ->
    ets:delete_object(hoax, E),
    ets:insert(hoax, E#expectation{call_count = C+1}).

lookup_expectations(Key) ->
    ets:lookup(hoax, Key).

format_unmet_expectation(#expectation{expected_count = undefined} = X) ->
    lists:flatten(hoax_fmt:fmt(X));
format_unmet_expectation(#expectation{call_count = C, expected_count = E} = X) ->
    lists:flatten(io_lib:format("~s [~b of ~b calls]", [hoax_fmt:fmt(X), C, E])).
