-module(slogan).

-export([main/1,
         bar/0,
         no_slogan/1,
         spec_slogan/1,
         spec_slogan/2,
         no_doc_slogan/1,
         spec_no_doc_slogan/1,
         spec_multiclause_slogan_ignored/1
        ]).

-doc "
main(Foo)

Returns ok.
".
-spec main(X :: integer()) -> ok.
main(_X) ->
    ok.

-doc "
foo()
Not a slogan since foo =/= bar
".
bar() ->
    ok.

-doc "
Not a slogan

Tests slogans in multi-clause
".
-spec no_slogan(atom()) -> atom();
               (term()) -> ok.
no_slogan(X) when is_atom(X) ->
    X;
no_slogan(_X) ->
    ok.

-spec spec_slogan(Y :: integer()) -> integer() | ok.
-doc "Not a slogan".
spec_slogan(_X) -> ok.

-spec spec_slogan(Y :: integer(), Z :: integer()) -> integer() | ok.
-doc "Not a slogan".
spec_slogan(_X, _Y) -> _X + _Y.

no_doc_slogan(X) -> X.

-spec spec_no_doc_slogan(Y) -> Y.
spec_no_doc_slogan(X) ->
    X.


-spec spec_multiclause_slogan_ignored(Y) -> Y;
                                     (Z) -> Z when Z :: integer().
spec_multiclause_slogan_ignored(X) ->
    X.
