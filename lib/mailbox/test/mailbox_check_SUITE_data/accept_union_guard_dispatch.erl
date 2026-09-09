-module(accept_union_guard_dispatch).

-export([handle/1]).

%% A normal type-test dispatch over a union argument. Both clauses are
%% reachable: is_integer selects the integer() member, is_atom the
%% atom() member. Neither should be rejected as unsatisfiable.
-spec handle(integer() | atom()) -> ok.
handle(X) when is_integer(X) -> ok;
handle(X) when is_atom(X) -> ok.
