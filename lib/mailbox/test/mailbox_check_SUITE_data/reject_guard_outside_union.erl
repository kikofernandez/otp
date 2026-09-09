-module(reject_guard_outside_union).

-export([f/1]).

%% The argument is integer() | atom(), but the guard requires a list.
%% No union member is a list, so the meet is none() and the clause is
%% unsatisfiable — it must be rejected.
-spec f(integer() | atom()) -> ok.
f(X) when is_list(X) -> ok.
