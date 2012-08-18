-module(hoax).

-include("hoax_api.hrl").
-export(?HOAX_API).
-ignore_xref(?HOAX_API).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    hoax_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% hoax API
%% ===================================================================

start() ->
    application:start(hoax).

stop() ->
    unload(),
    application:stop(hoax).

stub(M) -> stub(M, []).
stub(ModuleName, Expectations) ->
    purge_and_delete(ModuleName),
    Funcs = case module_exists(ModuleName) of
        false ->
            erlang:error({no_such_module_to_stub, ModuleName});
        true ->
            get_exports(ModuleName)
        end,
    mock(ModuleName, Funcs, Expectations).

fake(ModuleName, Expectations) ->
    case module_exists(ModuleName) of
        true ->
            erlang:error({module_exists, ModuleName});
        false ->
            Funcs = [{F,length(A)} || {F,A,_} <- Expectations],
            mock(ModuleName, Funcs, Expectations)
    end.

stub_a(B, M) -> stub_a(B, M, []).
stub_a(Behaviour, ModuleName, Expectations) ->
    Funcs = Behaviour:behaviour_info(callbacks),
    mock(ModuleName, Funcs, Expectations).

expect(Func, Args) -> expect(Func, Args, and_return(ok)).
expect(Func, Args, Action) -> {Func, Args, Action}.

and_return(Value) -> {return, Value}.

and_throw(Error) -> {throw, Error}.

unload() ->
    lists:foreach(fun unload/1, hoax_dict:get_mods()).

unload(ModuleName) ->
    case hoax_dict:has_mod(ModuleName) of
        true ->
            hoax_dict:del_mod(ModuleName),
            purge_and_delete(ModuleName);
        false ->
            erlang:error({not_hoaxed, ModuleName})
    end.

%%%%%%%%%%%%%

purge_and_delete(ModuleName) ->
    code:purge(ModuleName),
    code:delete(ModuleName).


mock(ModuleName, Funcs, Expectations) ->
    hoax_dict:add_mod(ModuleName),
    AST = hoax_ast:module(ModuleName, Funcs, Expectations),
    Forms = erl_syntax:revert_forms(AST),
    {ok, Mod, Bin} = compile:forms(Forms),
    code:load_binary(Mod, "", Bin).

module_exists(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {error, nofile} -> false;
        {module, ModuleName} -> true
    end.

get_exports(ModuleName) ->
    [E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info].

