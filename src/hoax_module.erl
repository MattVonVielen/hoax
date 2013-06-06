-module(hoax_module).

-export([generate/2]).

generate(Mod, Funcs) ->
    erl_syntax:revert_forms([
            module_attribute(Mod),
            export_attribute(Funcs) |
            [ function(Mod, Func, Arity) || {Func, Arity} <- Funcs ]
        ]).

module_attribute(Mod) ->
    erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Mod)]).

export_attribute(Funcs) ->
    Exports = [ erl_syntax:arity_qualifier(erl_syntax:atom(F), erl_syntax:integer(A))
        || {F,A} <- Funcs ],
    erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list(Exports)]).

function(Module, Function, Arity) ->
    Mod = erl_syntax:atom(Module),
    Func = erl_syntax:atom(Function),
    Vars = [ erl_syntax:variable([$V|integer_to_list(Num)]) || Num <- lists:seq(1,Arity) ],
    Handler = erl_syntax:module_qualifier(erl_syntax:atom(hoax_invocation),
                                          erl_syntax:atom(handle)),
    erl_syntax:function(Func, [
            erl_syntax:clause(Vars, [], [
                    erl_syntax:application(Handler, [ Mod, Func, erl_syntax:list(Vars) ])
                ])
        ]).
