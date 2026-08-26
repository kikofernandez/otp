-module(accept_literal_integer).
-export([f/0]).

%% A literal integer is a subtype of integer().
-spec f() -> integer().
f() -> 42.
