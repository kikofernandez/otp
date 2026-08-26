-module(reject_atom_vs_integer).
-export([f/0]).

%% An atom is not a subtype of integer().
-spec f() -> integer().
f() -> ok.
