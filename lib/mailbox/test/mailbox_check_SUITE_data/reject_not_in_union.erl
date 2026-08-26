-module(reject_not_in_union).
-export([f/0]).

%% other is not a member of ok | error.
-spec f() -> ok | error.
f() -> other.
