-module(hide_moduledoc2).

-export([main/0]).

-moduledoc hidden.

-doc "
Doc test module
".
-doc hidden.
main() ->
    ok().

-doc #{since => "1.0"}.
-doc hidden.
ok() ->
    ok.
