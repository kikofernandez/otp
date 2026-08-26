-module(reject_tuple_element).
-export([f/0]).

%% Second element ok is not an integer().
-spec f() -> {atom(), integer()}.
f() -> {tag, ok}.
