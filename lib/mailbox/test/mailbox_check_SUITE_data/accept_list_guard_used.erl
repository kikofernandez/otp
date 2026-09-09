-module(accept_list_guard_used).

-export([f/1]).

%% Argument is a list; guard confirms is_list; body returns the list.
%% Narrowing to list() must be compatible with the list() spec and the
%% return type, so the clause is accepted.
-spec f([integer()]) -> [integer()].
f(X) when is_list(X) -> X.
