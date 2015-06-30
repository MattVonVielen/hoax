-module(hoax_fmt).

-export([fmt/1]).

-include("hoax_int.hrl").

fmt(#expectation{key = {M,F,_}, expected_args = Args}) ->
    fmt({M, F, Args});
fmt({M, F, Args}) ->
    lists:flatten(io_lib:format("~s:~s(~s)", [M, F, format_args(Args)])).

format_args(Args) ->
    Formatted = lists:flatten(io_lib:format("~p", [Args])),
    string:sub_string(Formatted, 2, length(Formatted) - 1).
