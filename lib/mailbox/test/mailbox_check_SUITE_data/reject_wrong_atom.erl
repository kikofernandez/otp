-module(reject_wrong_atom).
-export([f/0]).

%% error is not the atom ok.
-spec f() -> ok.
f() -> error.
