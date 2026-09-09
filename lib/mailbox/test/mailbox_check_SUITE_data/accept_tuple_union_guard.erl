-module(accept_tuple_union_guard).

-export([f/1]).

%% The tuple carries a union element. Each clause's guard narrows that
%% element, and the tuple is rebuilt with the narrowed value. Requires
%% structural (element-wise) meet over tuples.
-spec f({tag, integer() | atom()}) -> {tag, integer() | atom()}.
f({tag, X}) when is_integer(X) -> {tag, X};
f({tag, X}) when is_atom(X) -> {tag, X}.
