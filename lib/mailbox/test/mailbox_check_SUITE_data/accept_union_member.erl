-module(accept_union_member).
-export([f/0]).

%% ok is a member of the union ok | error.
-spec f() -> ok | error.
f() -> ok.
