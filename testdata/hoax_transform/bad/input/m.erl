-module(m).

-compile([export_all, {parse_transform, hoax_transform}]).

f() ->
    hoax:expect(
        not_a_function_call
     ).

g() ->
    hoax:expect(receive
		   not_a_function_call_either -> output
	   end).
