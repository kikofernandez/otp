-module(accept_case).
-export([f/1]).

%% Every branch returns an atom in the declared union.
-spec f(ok | error) -> ok | error.
f(X) ->
    case X of
        ok -> ok;
        error -> error
    end.
