-module(reject_guard_contradicts_spec).

-export([start/1]).

%% The argument is declared as a tuple, but the guard requires a list.
%% list() and tuple() have no common value, so the clause is
%% unsatisfiable and must be rejected.
-spec start({atom(), any()}) -> error | ok.
start(X) when is_list(X) -> ok.
