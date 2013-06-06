-module(hoax_invocation).

-export([handle/3]).

handle(M, F, Args) ->
    case hoax_tab:lookup_action({M,F,Args}) of
        default         -> '$_hoax_default_return_$';
        {return, Value} -> Value;
        {Error, Reason} -> erlang:Error(Reason)
    end.

