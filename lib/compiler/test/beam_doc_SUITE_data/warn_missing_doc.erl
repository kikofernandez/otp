-module(warn_missing_doc).

-export([test/0, test/1, test/2]).
-export_type([test/0, test/1]).

-type test() :: ok.
-type test(N) :: N.

-callback test() -> ok.

test() -> ok.
test(N) -> N.

-doc #{ }.
test(N,M) -> N + M.
