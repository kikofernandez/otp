-module(reject_list_element).
-export([f/0]).

%% A list containing an atom is not a list of integers.
-spec f() -> [integer()].
f() -> [1, ok, 3].
