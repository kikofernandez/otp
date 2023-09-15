-module(docmodule_with_doc_attributes).

-export([main/0, ok/0, no_docs/0, with_file_docs/0]).

-moduledoc "
Moduledoc test module
".


-doc "
Doc test module
".
main() ->
    ok().


-doc #{authors => "Someone"}.
ok() ->
     no_docs().

no_docs() ->
    ok.

-doc {file, "README"}.
with_file_docs() ->
    ok.
