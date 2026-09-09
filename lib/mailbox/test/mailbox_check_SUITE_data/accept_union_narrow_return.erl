-module(accept_union_narrow_return).

-export([f/1]).

%% Guard narrows the union argument to its integer() member; returning
%% that value conforms to the integer() return type. Previously this was
%% wrongly rejected because the guard refinement on the scrutinee did not
%% reach the pattern variable X used in the body.
-spec f(integer() | atom()) -> integer().
f(X) when is_integer(X) -> X.
