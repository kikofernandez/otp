-module(reject_case_branch).
-export([f/1]).

%% The 'error' branch returns an atom, but the spec return is integer().
-spec f(ok | other) -> integer().
f(X) ->
    case X of
        ok -> 1;
        other -> bad
    end.
