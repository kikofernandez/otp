-module(accept_numeric_tower).
-export([f/0]).

%% integer literal <: integer() <: number().
-spec f() -> number().
f() -> 7.
