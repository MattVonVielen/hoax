-module(module_for_testing_fixtures).

-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

generators_should_not_appear_in_any_fixtures_test_() -> [].

neither_should_a_simple_function_test() -> ok.

setup() -> ok.
teardown(_) -> ok.
test_function1() -> ok.
test_function2() -> ok.

prefix1_test_function_1() -> ok.
prefix1_test_function_2() -> ok.
prefix1_setup() -> ok.
prefix1_teardown(_) -> ok.

prefix2_test_function_1() -> ok.
prefix2_test_function_2() -> ok.
prefix2_setup() -> ok.
prefix2_teardown(_) -> ok.
