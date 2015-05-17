-module(hoax_code_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("../src/hoax_int.hrl").

get_export_list_should_return_expected_function_list_when_module_cannot_be_loaded_test() ->
    Expectations = [
        #expectation{key={m,f,2}},
        #expectation{key={m,g,1}},
        #expectation{key={m,h,0}}
    ],
    ExpectedFunctionList = [{f,2},{g,1},{h,0}],
    ?assertEqual(ExpectedFunctionList, hoax_code:get_export_list(no_such_module, Expectations)).

get_export_list_should_return_exports_from_module_excluding_module_info_test() ->
    Exports = hoax_code:get_export_list(hoax_test_module, []),

    ?assertEqual(2, length(Exports)),
    ?assert(lists:member({function_one,2}, Exports)),
    ?assert(lists:member({function_two,1}, Exports)),
    ?assertNot(lists:member({module_info,0}, Exports)),
    ?assertNot(lists:member({module_info,1}, Exports)).

get_callback_list_should_throw_when_no_such_behaviour_test() ->
    ExpectedError = {no_such_behaviour_to_mock, no_such_behaviour},
    ?assertError(ExpectedError, hoax_code:get_callback_list(no_such_behaviour, no_such_module)).

get_callback_list_should_throw_when_not_a_behaviour_test() ->
    ExpectedError = {not_a_behaviour, hoax_test_module},
    ?assertError(ExpectedError, hoax_code:get_callback_list(hoax_test_module, no_such_module)).

get_callback_list_should_throw_when_mock_module_name_already_exists_test() ->
    ExpectedError = {module_exists, hoax_test_module},
    ?assertError(ExpectedError, hoax_code:get_callback_list(hoax_test_behaviour, hoax_test_module)).

get_callback_list_should_return_callbacks_from_behaviour_test() ->
    Callbacks = hoax_code:get_callback_list(hoax_test_behaviour, no_such_module),
    ?assertEqual(2, length(Callbacks)),
    ?assert(lists:member({callback_one,1}, Callbacks)),
    ?assert(lists:member({callback_two,2}, Callbacks)).

purge_and_delete_should_ensure_module_no_longer_loaded_and_restore_stickiness_test() ->
    code:ensure_loaded(hoax_test_module),
    code:stick_mod(hoax_test_module),

    hoax_code:purge_and_delete(hoax_test_module),

    ?assertMatch({file, _}, code:is_loaded(hoax_test_module)),
    ?assert(code:is_sticky(hoax_test_module)).
