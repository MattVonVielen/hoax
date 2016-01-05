-module(m).

-compile([export_all, {parse_transform, hoax_transform}]).

f() ->
    expect(
        not_a_function_call
     ).

g() ->
    expect(receive
		   not_a_function_call_either -> output
	   end).