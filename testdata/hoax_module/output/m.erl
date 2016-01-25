-module(m).

-export([f/2, g/1, h/0]).

f(V1, V2) -> hoax_invocation:handle(m, f, [V1, V2]).

g(V1) -> hoax_invocation:handle(m, g, [V1]).

h() -> hoax_invocation:handle(m, h, []).