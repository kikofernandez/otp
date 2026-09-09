-module(accept_unused_narrowed_arg).

-export([f/1]).

%% The narrowed argument is not used in the body. Even if internal
%% narrowing produced a none()-typed temporary, an unused binding must
%% not cause the clause to be rejected.
-spec f(integer()) -> ok.
f(_X) when is_integer(_X) -> ok.
