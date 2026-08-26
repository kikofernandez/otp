-module(accept_var_passthrough).
-export([f/1]).

%% The argument's spec type flows straight to the return.
-spec f(integer()) -> integer().
f(X) -> X.
