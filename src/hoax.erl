-module(hoax).

-include("hoax_api.hrl").
-export(?HOAX_API).
-ignore_xref(?HOAX_API).

%% ===================================================================
%% hoax API
%% ===================================================================

start() ->
    hoax_srv:start().

stop() ->
    case erlang:whereis(hoax_srv) of
        undefined -> ok;
        _ ->
            lists:foreach(
                fun hoax_code:purge_and_delete/1,
                hoax_srv:stop())
    end.

stub(M) -> stub(M, []).
stub(ModuleName, Expectations) ->
    hoax_code:purge_and_delete(ModuleName),

    hoax_code:module_exists(ModuleName) orelse
            error({no_such_module_to_stub, ModuleName}),

    make_hoax(ModuleName, hoax_code:get_exports(ModuleName), Expectations).

fake(ModuleName, Expectations) ->
    hoax_code:module_exists(ModuleName) andalso
            error({module_exists, ModuleName}),

    make_hoax(ModuleName, hoax_code:expectations_to_funcs(Expectations), Expectations).

%stub_a(B, M) -> stub_a(B, M, []).
%stub_a(Behaviour, ModuleName, Expectations) ->
%    Funcs = Behaviour:behaviour_info(callbacks),
%    make_hoax(ModuleName, Funcs, Expectations).

expect(Func, Args) -> expect(Func, Args, and_return(ok)).
expect(Func, Args, Action) -> {Func, Args, Action}.

and_return(Value) -> {return, Value}.

and_throw(Error) -> {throw, Error}.

%%%%%%%%%%%%%

make_hoax(ModuleName, Funcs, Expectations) ->
    hoax_srv:add_mod(ModuleName),
    Forms = hoax_ast:module(ModuleName, Funcs, Expectations, permissive),
    hoax_code:compile(Forms).
