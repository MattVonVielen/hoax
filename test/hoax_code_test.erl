-module(hoax_code_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

get_function_list_should_throw_when_module_cannot_be_loaded_test() ->
    ExpectedError = {no_such_module_to_mock, no_such_module},
    ?assertError(ExpectedError, hoax_code:get_function_list(no_such_module)).

get_function_list_should_return_exports_from_module_excluding_module_info_test() ->
    Exports = hoax_code:get_function_list(hoax_test_module),

    ?assertEqual(2, length(Exports)),
    ?assert(lists:member({function_one,2}, Exports)),
    ?assert(lists:member({function_two,1}, Exports)),
    ?assertNot(lists:member({module_info,0}, Exports)),
    ?assertNot(lists:member({module_info,1}, Exports)).

get_function_list_should_throw_when_no_such_behaviour_test() ->
    ExpectedError = {no_such_behaviour_to_mock, no_such_behaviour},
    ?assertError(ExpectedError, hoax_code:get_function_list(no_such_behaviour, no_such_module)).

get_function_list_should_throw_when_not_a_behaviour_test() ->
    ExpectedError = {not_a_behaviour, hoax_test_module},
    ?assertError(ExpectedError, hoax_code:get_function_list(hoax_test_module, no_such_module)).

get_function_list_should_throw_when_mock_module_name_already_exists_test() ->
    ExpectedError = {module_exists, hoax_test_module},
    ?assertError(ExpectedError, hoax_code:get_function_list(hoax_test_behaviour, hoax_test_module)).

get_function_list_should_return_callbacks_from_behaviour_test() ->
    Callbacks = hoax_code:get_function_list(hoax_test_behaviour, no_such_module),
    ?assertEqual(2, length(Callbacks)),
    ?assert(lists:member({callback_one,1}, Callbacks)),
    ?assert(lists:member({callback_two,2}, Callbacks)).

purge_and_delete_should_ensure_module_no_longer_loaded_test() ->
    code:ensure_loaded(hoax_test_module),

    hoax_code:purge_and_delete(hoax_test_module),

    ?assertEqual(false, code:is_loaded(hoax_test_module)).
