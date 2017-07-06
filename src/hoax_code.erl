-module(hoax_code).

-export([
        get_export_list/2,
        get_callback_list/2,
        purge_and_delete/1,
        compile/2
    ]).

-include("hoax_int.hrl").

get_export_list(ModuleName, ExpectedFunctions) ->
    case module_exists(ModuleName) of
        true ->
            [E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info];
        false ->
            [ {F, A} || #expectation{key = {_,F,A}} <- ExpectedFunctions ]
    end.

get_callback_list(Behaviour, ModuleName) ->
    module_exists(ModuleName) andalso
        error({module_exists, ModuleName}),
    module_exists(Behaviour) orelse
        error({no_such_behaviour_to_mock, Behaviour}),
    erlang:function_exported(Behaviour, behaviour_info, 1) orelse
        error({not_a_behaviour, Behaviour}),
    Behaviour:behaviour_info(callbacks).

backup_coverdata(ModName) ->
    case cover:is_compiled(ModName) of
        false ->
            ok;
        _Other ->
            FileName = coverdata_filename(ModName),
            cover:export(FileName, ModName)
    end.

coverdata_filename(ModName) ->
    %% This assumes we start and stop in the same directory.
    {ok, D} = file:get_cwd(),
    filename:join(D, atom_to_list(ModName) ++ ".coverdata").

restore_coverdata(ModName) ->
    File = coverdata_filename(ModName),
    case cover:import(File) of
        {error, {cant_open_file, File, enoent}} ->
            ok;
        ok ->
            file:delete(File),
            ok;
        Other ->
            Other
    end.

has_coverdata_backup(ModName) ->
    Filename = coverdata_filename(ModName),
    filelib:is_file(Filename).

purge_and_delete(ModuleName) ->
    Sticky = code:is_sticky(ModuleName),
    code:purge(ModuleName),
    code:delete(ModuleName),
    code:ensure_loaded(ModuleName),
    case has_coverdata_backup(ModuleName) of
        true ->
            code:unstick_mod(ModuleName),
            File = beam_path(ModuleName),
            {ok, ModuleName} = cover:compile_beam(File),
            restore_coverdata(ModuleName);
        _ ->
            ok
    end,
    restore_stickiness(ModuleName, Sticky).

beam_path(ModuleName) ->
    {_, _, Filename} = code:get_object_code(ModuleName),
    Filename.

module_exists(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {error, nofile} -> false;
        {module, ModuleName} -> true
    end.

restore_stickiness(ModuleName, true) ->
    code:stick_mod(ModuleName);
restore_stickiness(_, _) ->
    ok.

compile(Mod, Forms) ->
    Sticky = code:is_sticky(Mod),
    {ok, Mod, Bin} = compile:forms(Forms),
    backup_coverdata(Mod),
    code:unstick_mod(Mod),
    code:load_binary(Mod, "", Bin),
    restore_stickiness(Mod, Sticky).
