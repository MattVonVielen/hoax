-module(hoax_test_module).

-export([function_one/2, function_two/1]).

function_one(A, B) ->
    {function_one, A, B}.

function_two(A) ->
    {function_two, A}.
