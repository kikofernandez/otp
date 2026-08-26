-module(reject_apply_return).
-export([f/0]).

%% g/0 returns an atom, but f/0 promises an integer().
-spec f() -> integer().
f() -> g().

-spec g() -> atom().
g() -> ok.
