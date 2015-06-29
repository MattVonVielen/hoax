-module(module_for_testing_fixtures).

-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

generators_should_not_appear_in_any_fixtures_test_() -> [].

neither_should_a_simple_function_test() -> ok.

setup() -> erlang:put(setup_called, true).
teardown(Arg) -> erlang:put(teardown_called, {true, Arg}).
test_function1() -> ok.
test_function2() -> ok.

prefix1_test_function_1() -> ok.
prefix1_test_function_2() -> ok.
prefix1_setup() -> erlang:put(prefix1_setup_called, true).
prefix1_teardown(Arg) -> erlang:put(prefix1_teardown_called, {true, Arg}).

prefix2_test_function_1() -> ok.
prefix2_test_function_2() -> ok.
prefix2_setup() -> erlang:put(prefix2_setup_called, true).
prefix2_teardown(Arg) -> erlang:put(prefix2_teardown_called, {true, Arg}).

parameterized_test_function1(_) -> ok.
parameterized_test_function2(_) -> ok.

prefix1_parameterized_test_function_1(_) -> ok.
prefix1_parameterized_test_function_2(_) -> ok.

prefix2_parameterized_test_function_1(_) -> ok.
prefix2_parameterized_test_function_2(_) -> ok.
