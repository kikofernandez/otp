-module(accept_unspec_dynamic).
-export([f/1, g/0]).

%% g/0 has no spec, so its return defaults to dynamic(); calling it and
%% returning the result checks against any expected type (gradual).
-spec f() -> integer().
f() -> g().

g() -> hd([1, 2, 3]).

%% keep the second arity to avoid unused warnings interfering
f(_X) -> ok.
