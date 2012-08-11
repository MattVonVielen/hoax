-module(hoax_test_module).

-export([exported_function/2]).

exported_function(A, B) ->
    {exported_function, A, B}.

unexported_function(A, B) ->
    {unexported_function, A, B}.
