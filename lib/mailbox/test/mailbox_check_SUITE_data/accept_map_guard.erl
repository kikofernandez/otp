-module(accept_map_guard).

-export([f/1]).

%% A structural map value satisfies is_map, so the clause is reachable.
-spec f(#{atom() => integer()}) -> ok.
f(X) when is_map(X) -> ok.
