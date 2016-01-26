-module(m).

-compile([export_all, {parse_transform, hoax_transform}]).

hoax_syntax_examples() ->
    V1 = two,
    V2 = 3,
    hoax:test(fun () ->
		      hoax:expect([{expectation, {module, function, 0}, [], [], default, 0, undefined},
				   {expectation, {module, function2, 1}, ['_'], [], default, 0, undefined}]),
		      hoax:expect([{expectation, {module, function, 1}, [one], [], fun (_1) -> arbitrary_return_value end, 0,
				    undefined},
				   {expectation, {module, function, 2}, [V1, V2], [], fun (_1, _2) -> error(some_error) end, 0,
				    undefined}])
	      end).