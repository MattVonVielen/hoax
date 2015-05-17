-module(hoax_tab_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("../src/hoax_int.hrl").

-define(_test(Title, Expr), {Title, ?LINE, fun() -> Expr end}).

unmet_expectations_fixture_test_() ->
    {foreach,
     fun() -> hoax_tab:create() end,
     fun(_) -> hoax_tab:delete() end,
     [ ?_test("Include expectation when not called", begin
                    hoax_tab:insert(#expectation{key={m,f,2}, args=[1,2]}),
                    ?assertEqual(["m:f(1,2)"], hoax_tab:unmet_expectations()) end),
      ?_test("Include expectation when not called enough times", begin
                    hoax_tab:insert(#expectation{key={m,f,2}, args=[1,2],
                                                 expected_count=2, call_count=1}),
                    ?assertEqual(["m:f(1,2) [1 of 2 calls]"], hoax_tab:unmet_expectations()) end),
      ?_test("Omit expectation when called correct number of times", begin
                    hoax_tab:insert(#expectation{key={m,f,2}, args=[1,2],
                                                 expected_count=2, call_count=2}),
                    ?assertEqual([], hoax_tab:unmet_expectations()) end),
      ?_test("Omit expectation when called once and no expected count given", begin
                    hoax_tab:insert(#expectation{key={m,f,2}, args=[1,2],
                                                 call_count=1}),
                    ?assertEqual([], hoax_tab:unmet_expectations()) end),
      ?_test("Omit expectation when called many times and no expected count given", begin
                    hoax_tab:insert(#expectation{key={m,f,2}, args=[1,2],
                                                 call_count=6}),
                    ?assertEqual([], hoax_tab:unmet_expectations()) end),
      ?_test("Omit expectation when never called and expected count is zero", begin
                    hoax_tab:insert(#expectation{key={m,f,2}, args=[1,2],
                                                 expected_count=0}),
                    ?assertEqual([], hoax_tab:unmet_expectations()) end) ]
    }.
