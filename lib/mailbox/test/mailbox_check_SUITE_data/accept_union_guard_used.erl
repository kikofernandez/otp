-module(accept_union_guard_used).

-export([f/1]).

%% Both clauses reachable and each returns the narrowed value. This
%% exercises narrowing that reaches the body for each union member.
-spec f(integer() | atom()) -> integer() | atom().
f(X) when is_integer(X) -> X;
f(X) when is_atom(X) -> X.
