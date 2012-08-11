-module(hoax_test).

-compile([export_all]).
-import(hoax, [stub/2, expect/2, expect/3, and_return/1]).

-include_lib("eunit/include/eunit.hrl").
-define(assertIsExit(Error, Actual), ?assertMatch({'EXIT', {Error, _}}, Actual)).

stub_should_throw_when_module_does_not_have_expected_function_test() ->
    Result = (catch stub(hoax_test_module, [
                expect(nonexistent_function, [])
            ])),
    ?assertIsExit({no_such_function_to_stub, {nonexistent_function, 0}}, Result).
