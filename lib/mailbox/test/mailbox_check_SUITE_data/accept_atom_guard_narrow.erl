-module(accept_atom_guard_narrow).

-export([f/1]).

%% is_atom narrows the union argument to its atom member; returning that
%% value conforms to the atom() return type. Requires both the is_atom
%% guard BIF mapping and union-aware meet.
-spec f(integer() | atom()) -> atom().
f(X) when is_atom(X) -> X.
