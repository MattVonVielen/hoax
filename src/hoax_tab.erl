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

lookup_action(M, F, Args) ->
    case ets:lookup(hoax, {M, F, length(Args)}) of
        [] ->
            {error, {unexpected_invocation, {M, F, Args}}};
        Records ->
            find_matching_args(M, F, Args, Records)
    end.

find_matching_args(M, F, Args, [#expectation{args=Args,call_count=X,expected_count=X} |_]) ->
    {error, {too_many_invocations, {M, F, Args}}};
find_matching_args(_, _, Args, [Expectation = #expectation{args=Args, call_count=C}|_]) ->
%    ets:update_counter(hoax, Key, {#expectation.call_count, 1}),
    ets:delete_object(hoax, Expectation),
    ets:insert(hoax, Expectation#expectation{call_count = C+1}),
    Expectation#expectation.action;
find_matching_args(M, F, Args, [ _ | Rest ]) ->
    find_matching_args(M, F, Args, Rest);
find_matching_args(M, F, Args, []) ->
    {error, {unexpected_arguments, {M, F, Args}}}.
