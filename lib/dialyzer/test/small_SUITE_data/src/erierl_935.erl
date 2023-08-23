-module(erierl_935).

-export([repeated_annotated_variables_different_types/1,
        repeated_annotated_variables_different_types_in_when1/1,
        repeated_annotated_variables_different_types_in_when2/1,
        shared_type_variable_in_when_clause/1]).

-spec repeated_annotated_variables_different_types(X :: integer()) -> X.
repeated_annotated_variables_different_types(X) ->
    X.

-spec repeated_annotated_variables_different_types_in_when1(X) -> X :: atom() when X :: integer().
repeated_annotated_variables_different_types_in_when1(X) ->
    X.

-spec repeated_annotated_variables_different_types_in_when2(X :: integer()) -> X when X :: integer().
repeated_annotated_variables_different_types_in_when2(X) ->
    X.

-spec shared_type_variable_in_when_clause(X) -> X when X :: integer().
shared_type_variable_in_when_clause(X) ->
    X.
