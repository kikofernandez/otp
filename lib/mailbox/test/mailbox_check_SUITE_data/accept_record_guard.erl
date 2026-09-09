-module(accept_record_guard).

-export([f/1]).

%% A record is a tagged tuple at runtime, so an is_record guard on a
%% value declared as tuple() is satisfiable and must be accepted.
-spec f(tuple()) -> ok.
f(X) when is_tuple(X) -> ok.
