-module(accept_list).
-export([f/0]).

%% [1,2,3] normalizes to a list of integers, subtype of [integer()].
-spec f() -> [integer()].
f() -> [1, 2, 3].
