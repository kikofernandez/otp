-module(accept_let_single).

-export([f/1]).

%% A single-variable let: Y is bound to foo(X) and returned. Exercises
%% the dominant Core `let` form (one var, single-valued argument).
-spec f(integer()) -> integer().
f(X) ->
    Y = foo(X),
    Y.

-spec foo(integer()) -> integer().
foo(A) -> A.
