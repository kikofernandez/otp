-module(accept_tuple).
-export([f/0]).

%% {ok, 1} checks against {atom(), integer()} pointwise.
-spec f() -> {atom(), integer()}.
f() -> {ok, 1}.
