-module(reject_let_return_mismatch).

-export([f/1]).

%% Y is bound (via a let) to tag(X) :: atom(), then returned where the
%% spec promises integer(). The let-bound variable must carry atom(),
%% so returning it must be rejected.
-spec f(integer()) -> integer().
f(X) ->
    Y = tag(X),
    Y.

-spec tag(integer()) -> atom().
tag(_A) -> ok.
