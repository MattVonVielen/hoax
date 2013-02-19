-module(hoax_code_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

module_exists_should_handle_nonexistent_module_test() ->
    ?assertNot(hoax_code:module_exists(no_such_module)).

module_exists_should_handle_not_yet_loaded_module_test() ->
    code:purge(hoax_test_module),
    code:delete(hoax_test_module),

    ?assert(hoax_code:module_exists(hoax_test_module)).

module_exists_should_handle_loaded_module_test() ->
    {module, hoax_test_module} = code:ensure_loaded(hoax_test_module),

    ?assert(hoax_code:module_exists(hoax_test_module)).

get_exports_should_return_exports_from_module_excluding_module_info_test() ->
    Exports = hoax_code:get_exports(hoax_test_module),

    ?assertEqual(2, length(Exports)),
    ?assert(lists:member({function_one,2}, Exports)),
    ?assert(lists:member({function_two,1}, Exports)),
    ?assertNot(lists:member({module_info,0}, Exports)),
    ?assertNot(lists:member({module_info,1}, Exports)).

get_callbacks_should_return_error_when_no_such_behaviour_test() ->
    ExpectedError = {error, {no_such_behaviour_to_mock, no_such_module}},
    ?assertEqual(ExpectedError, hoax_code:get_callbacks(no_such_module)).

get_callbacks_should_return_error_when_not_a_behaviour_test() ->
    ExpectedError = {error, {not_a_behaviour, hoax_test_module}},
    ?assertEqual(ExpectedError, hoax_code:get_callbacks(hoax_test_module)).

get_callbacks_should_return_callbacks_from_behaviour_test() ->
    {ok, Callbacks} = hoax_code:get_callbacks(hoax_test_behaviour),
    ?assertEqual(2, length(Callbacks)),
    ?assert(lists:member({callback_one,1}, Callbacks)),
    ?assert(lists:member({callback_two,2}, Callbacks)).

purge_and_delete_should_ensure_module_no_longer_loaded_test() ->
    code:ensure_loaded(hoax_test_module),

    hoax_code:purge_and_delete(hoax_test_module),

    ?assertEqual(false, code:is_loaded(hoax_test_module)).
