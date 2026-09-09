-module(accept_binary_guard_narrow).

-export([f/1]).

%% is_binary narrows the union argument to its binary member.
-spec f(binary() | atom()) -> binary().
f(X) when is_binary(X) -> X.
