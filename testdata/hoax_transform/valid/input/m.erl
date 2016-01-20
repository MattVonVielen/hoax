-module(m).

-compile([export_all, {parse_transform, hoax_transform}]).

hoax_syntax_examples() ->
    V1 = two,
    V2 = 3,
    hoax:test(fun() ->
                      hoax:expect(module:function(), module:function2(_)),
                      hoax:expect(receive
                                 module:function(one) -> arbitrary_return_value;
                                 module:function(V1, V2) -> error(some_error)
                             end)
              end).
