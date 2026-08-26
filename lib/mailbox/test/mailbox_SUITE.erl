%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%% Test suite for the mailbox type checker.
%%
%% Focuses on the pure units that are implemented and testable in
%% isolation: the type representation (`mailbox_types`), literal
%% construction, list normalization, the subtyping relation, type
%% formatting, and the parser (`mailbox_parser`) that converts
%% erl_parse abstract type forms into internal `c_types()` records.
%%
-module(mailbox_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../src/c_types.hrl").

-export([all/0, groups/0]).
-export([% mailbox_types: constructors / literals
         build_lit_atom/1, build_lit_integer/1, build_lit_list/1,
         build_lit_tuple/1, lit_kind/1,
         % mailbox_types: subtyping
         subtype_reflexive/1, subtype_literal_general/1,
         subtype_numeric_tower/1, subtype_dynamic_both_ways/1,
         subtype_union/1, subtype_list_covariant/1,
         subtype_tuple_pointwise/1, subtype_negative/1,
         % mailbox_types: meet
         meet_precise/1, meet_incompatible/1,
         % mailbox_types: fun accessors
         fun_return_and_args/1,
         % mailbox_types: formatting
         format_atom_literal/1, format_general/1,
         format_list/1, format_union/1,
         % mailbox_parser
         parse_atom_literal/1, parse_builtin/1, parse_union/1,
         parse_list/1]).

all() ->
    [{group, types}, {group, subtyping}, {group, meet},
     {group, fun_accessors}, {group, formatting}, {group, parser}].

groups() ->
    [{types, [parallel],
      [build_lit_atom, build_lit_integer, build_lit_list,
       build_lit_tuple, lit_kind]},
     {subtyping, [parallel],
      [subtype_reflexive, subtype_literal_general, subtype_numeric_tower,
       subtype_dynamic_both_ways, subtype_union, subtype_list_covariant,
       subtype_tuple_pointwise, subtype_negative]},
     {meet, [parallel],
      [meet_precise, meet_incompatible]},
     {fun_accessors, [parallel],
      [fun_return_and_args]},
     {formatting, [parallel],
      [format_atom_literal, format_general, format_list, format_union]},
     {parser, [parallel],
      [parse_atom_literal, parse_builtin, parse_union, parse_list]}].

%%% ===================================================================
%%% mailbox_types: literal construction
%%% ===================================================================

build_lit_atom(_Config) ->
    #litTy{kind = atom, val = ok} = mailbox_types:build_lit(ok),
    ok.

build_lit_integer(_Config) ->
    #litTy{kind = integer, val = 42} = mailbox_types:build_lit(42),
    ok.

build_lit_list(_Config) ->
    %% [1,2] normalizes to a uniform list of the element-type union.
    #builtTy{builtIn = list, args = [Elem]} = mailbox_types:build_lit([1, 2]),
    %% element type must accept both 1 and 2
    true = mailbox_types:is_subtype(mailbox_types:build_lit(1), Elem),
    true = mailbox_types:is_subtype(mailbox_types:build_lit(2), Elem),
    %% empty list is the dedicated empty-list type
    #emptyListTy{} = mailbox_types:build_lit([]),
    ok.

build_lit_tuple(_Config) ->
    #tupleTy{args = [A, B]} = mailbox_types:build_lit({ok, 1}),
    #litTy{kind = atom, val = ok} = A,
    #litTy{kind = integer, val = 1} = B,
    ok.

lit_kind(_Config) ->
    atom = mailbox_types:lit_kind(mailbox_types:build_lit(ok)),
    integer = mailbox_types:lit_kind(mailbox_types:build_lit(7)),
    ok.

%%% ===================================================================
%%% mailbox_types: subtyping
%%% ===================================================================

subtype_reflexive(_Config) ->
    %% Reflexivity must hold even when annotations differ.
    A = #litTy{anno = 0, kind = atom, val = ok},
    B = #litTy{anno = {12, 3}, kind = atom, val = ok},
    true = mailbox_types:is_subtype(A, B),
    true = mailbox_types:is_subtype(B, A),
    ok.

subtype_literal_general(_Config) ->
    %% ok <: atom()
    true = mailbox_types:is_subtype(mailbox_types:build_lit(ok),
                                    mailbox_types:any_type()),
    true = mailbox_types:is_subtype(#litTy{kind = atom, val = ok},
                                    #builtTy{builtIn = atom}),
    true = mailbox_types:is_subtype(#litTy{kind = integer, val = 1},
                                    mailbox_types:integer_type()),
    ok.

subtype_numeric_tower(_Config) ->
    %% integer() <: number(), float() <: number()
    true = mailbox_types:is_subtype(mailbox_types:integer_type(),
                                    mailbox_types:number_type()),
    true = mailbox_types:is_subtype(mailbox_types:float_type(),
                                    mailbox_types:number_type()),
    ok.

subtype_dynamic_both_ways(_Config) ->
    %% dynamic() is compatible in both directions (gradual typing).
    Dyn = mailbox_types:dyn_type(),
    true = mailbox_types:is_subtype(Dyn, mailbox_types:integer_type()),
    true = mailbox_types:is_subtype(mailbox_types:integer_type(), Dyn),
    ok.

subtype_union(_Config) ->
    %% ok <: ok | error ; and (ok | error) <: atom()
    Union = #unionTy{args = [#litTy{kind = atom, val = ok},
                             #litTy{kind = atom, val = error}]},
    true = mailbox_types:is_subtype(#litTy{kind = atom, val = ok}, Union),
    true = mailbox_types:is_subtype(Union, #builtTy{builtIn = atom}),
    %% integer literal is NOT a member of an all-atom union
    false = mailbox_types:is_subtype(#litTy{kind = integer, val = 1}, Union),
    ok.

subtype_list_covariant(_Config) ->
    %% [integer()] <: [number()]
    LI = mailbox_types:list_of([mailbox_types:integer_type()]),
    LN = mailbox_types:list_of([mailbox_types:number_type()]),
    true = mailbox_types:is_subtype(LI, LN),
    %% any concrete list <: list() (0-arg "any list")
    true = mailbox_types:is_subtype(LI, #builtTy{builtIn = list}),
    %% [] <: any list
    true = mailbox_types:is_subtype(#emptyListTy{}, #builtTy{builtIn = list}),
    ok.

subtype_tuple_pointwise(_Config) ->
    %% {ok, integer()} <: {atom(), number()}
    T1 = #tupleTy{args = [#litTy{kind = atom, val = ok},
                          mailbox_types:integer_type()]},
    T2 = #tupleTy{args = [#builtTy{builtIn = atom},
                          mailbox_types:number_type()]},
    true = mailbox_types:is_subtype(T1, T2),
    %% arity mismatch is not a subtype
    T3 = #tupleTy{args = [#builtTy{builtIn = atom}]},
    false = mailbox_types:is_subtype(T1, T3),
    ok.

subtype_negative(_Config) ->
    %% atom() is NOT a subtype of integer()
    false = mailbox_types:is_subtype(#builtTy{builtIn = atom},
                                     mailbox_types:integer_type()),
    ok.

%%% ===================================================================
%%% mailbox_types: meet
%%% ===================================================================

meet_precise(_Config) ->
    %% meet(tuple(), {a,b}) keeps the more precise {a,b}.
    Any = mailbox_types:tuple_type(),  %% #tupleAnyTy{}
    Concrete = #tupleTy{args = [#litTy{kind = atom, val = a},
                                #litTy{kind = atom, val = b}]},
    Concrete = mailbox_types:meet(Any, Concrete),
    Concrete = mailbox_types:meet(Concrete, Any),
    ok.

meet_incompatible(_Config) ->
    %% meet of two unrelated types is none().
    #builtTy{builtIn = none} =
        mailbox_types:meet(#builtTy{builtIn = atom},
                           mailbox_types:integer_type()),
    ok.

%%% ===================================================================
%%% mailbox_types: fun accessors
%%% ===================================================================

fun_return_and_args(_Config) ->
    Args = [mailbox_types:integer_type()],
    Ret = mailbox_types:integer_type(),
    F = mailbox_types:fun_type(Args, Ret),
    Args = mailbox_types:fun_args(F),
    Ret = mailbox_types:fun_return(F),
    ok.

%%% ===================================================================
%%% mailbox_types: formatting
%%% ===================================================================

format_atom_literal(_Config) ->
    "ok" = flat(mailbox_types:format_type(#litTy{kind = atom, val = ok})),
    "42" = flat(mailbox_types:format_type(#litTy{kind = integer, val = 42})),
    ok.

format_general(_Config) ->
    "atom()" = flat(mailbox_types:format_type(#builtTy{builtIn = atom})),
    "integer()" = flat(mailbox_types:format_type(mailbox_types:integer_type())),
    ok.

format_list(_Config) ->
    L = mailbox_types:list_of([mailbox_types:integer_type()]),
    "[integer()]" = flat(mailbox_types:format_type(L)),
    ok.

format_union(_Config) ->
    U = #unionTy{args = [#litTy{kind = atom, val = ok},
                         #litTy{kind = atom, val = error}]},
    "ok | error" = flat(mailbox_types:format_type(U)),
    ok.

%%% ===================================================================
%%% mailbox_parser: abstract format -> c_types()
%%% ===================================================================

parse_atom_literal(_Config) ->
    %% -type ... :: ok.  ->  litTy(atom, ok)
    T = parse_type("ok"),
    #litTy{kind = atom, val = ok} = T,
    ok.

parse_builtin(_Config) ->
    #builtTy{builtIn = integer} = parse_type("integer()"),
    ok.

parse_union(_Config) ->
    #unionTy{args = Args} = parse_type("ok | error"),
    2 = length(Args),
    ok.

parse_list(_Config) ->
    #builtTy{builtIn = list, args = [_]} = parse_type("[integer()]"),
    ok.

%%% ===================================================================
%%% Helpers
%%% ===================================================================

flat(IoList) -> lists:flatten(IoList).

%% Parse a type expression string into a c_types() via the same path
%% mailbox_parser expects: erl_parse abstract type form -> from_abs_type/1.
%% We wrap the type in a `-type` attribute and extract the type body,
%% since erl_parse does not export parse_type/1.
parse_type(Str) ->
    Form = "-type t() :: " ++ Str ++ ".",
    {ok, Tokens, _} = erl_scan:string(Form),
    {ok, {attribute, _, type, {t, AbsType, []}}} = erl_parse:parse_form(Tokens),
    mailbox_parser:from_abs_type(AbsType).
