-module(accept_apply_local).
-export([f/0]).

%% f/0 calls g/0; g's spec return (ok) satisfies f's spec return.
-spec f() -> ok.
f() -> g().

-spec g() -> ok.
g() -> ok.
