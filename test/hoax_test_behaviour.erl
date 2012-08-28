-module(hoax_test_behaviour).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{callback_one, 1}, {callback_two, 2}].
