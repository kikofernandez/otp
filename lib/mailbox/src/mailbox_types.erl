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
%% Helper module to perform operations on types
%%

-module(mailbox_types).
-moduledoc false.

-export([combine_fun_types/1, apply_subst/2, is_subtype/2, meet/2]).
-export([fun_return/1, fun_args/1]).
-export([format_type/1]).
-export([any_type/0, build_lit/1, lit_kind/1, dyn_type/0, fun_type/2]).
-export([none_type/0, is_none/1]).
-export([is_dynamic/1]).
-export([bin_type/0, bitstring_type/0, boolean_type/0,
         atom_type/0,
         float_type/0, function_type/0, integer_type/0,
         list_type/0, map_type/0, number_type/0,
         pid_type/0, port_type/0, record_type/0,
         reference_type/0, tuple_type/0]).
-export([tuple_type/1, list_type/1, list_of/1, cons/2, union/1]).

-include("c_types.hrl").

-doc "Builds the gradual `dynamic()` type (compatible with any type).".
-spec dyn_type() -> c_types().
dyn_type() -> #builtTy{anno=0, builtIn = 'dynamic'}.

-doc "Builds the empty type `none()` (the bottom type, no values).".
-spec none_type() -> c_types().
none_type() -> #builtTy{anno = 0, builtIn = 'none'}.

-doc "Returns `true` if the type is the empty type `none()`.".
-spec is_none(c_types()) -> boolean().
is_none(#builtTy{builtIn = 'none'}) -> true;
is_none(_) -> false.

-doc "Returns `true` if the type is the gradual `dynamic()` type.".
-spec is_dynamic(c_types()) -> boolean().
is_dynamic(#builtTy{builtIn = 'dynamic'}) -> true;
is_dynamic(_) -> false.

-doc "Builds a function type.".
-spec fun_type([c_types()], c_types()) -> c_types().
fun_type(Args, Return) ->
    #funTy{anno=0, args=Args, return=Return}.

-doc """
Builds the type any().
""".
-spec any_type() -> c_types().
any_type()       -> #builtTy{anno = 0, builtIn = 'any'}.

-doc "Builds the `atom()` type.".
-spec atom_type() -> c_types().
atom_type()      -> #builtTy{builtIn = atom}.

-doc "Builds the `binary()` type.".
-spec bin_type() -> c_types().
bin_type()       -> #builtTy{builtIn = binary}.

-doc "Builds the `bitstring()` type.".
-spec bitstring_type() -> c_types().
bitstring_type()       -> #bitString{}.

-doc "Builds the `boolean()` type.".
-spec boolean_type() -> c_types().
boolean_type()       -> #builtTy{builtIn='boolean'}.

-doc "Builds the `float()` type.".
-spec float_type() -> c_types().
float_type()       -> #builtTy{builtIn = float}.

-doc "Builds the `fun()` (any function) type.".
-spec function_type() -> c_types().
function_type()       -> #funAbsTy{}.

-doc """
Builds the `integer()` type.
""".
-spec integer_type() -> c_types().
integer_type()       -> #builtTy{builtIn = integer}.

-doc "Builds the `list()` (any list) type.".
-spec list_type() -> c_types().
list_type()       -> #builtTy{builtIn = list}.

-doc "Builds a `list` type with the given raw argument list.".
-spec list_type(Args :: [c_types()]) -> c_types().
list_type(Args)       -> #builtTy{builtIn= list, args = Args}.

-doc """
Build a uniform list type whose element type is the union of the
given types. `list_of([])` yields the empty list type.
""".
-spec list_of([c_types()]) -> c_types().
list_of([]) ->
    #emptyListTy{};
list_of(Types) ->
    #builtTy{builtIn = 'list', args = [make_union(0, Types)]}.

-doc """
Prepend a head element type onto a tail list type, producing a
uniform list. The element type becomes the union of the head type
and the tail's element type(s).
""".
-spec cons(HeadTy :: c_types(), TailTy :: c_types()) -> c_types().
cons(HeadTy, TailTy) ->
    list_of([HeadTy | tail_elem(TailTy)]).

-spec union(Args :: [c_types()]) -> c_types().
union(Types) when is_list(Types) ->
    #unionTy{args = Types}.

%% Extract the element type(s) contributed by a list tail.
tail_elem(#emptyListTy{})                          -> [];
tail_elem(#builtTy{builtIn = list, args = [Elem]}) -> [Elem];
tail_elem(#builtTy{builtIn = list, args = []})     -> [any_type()];
tail_elem(Other)                                   -> [Other].  %% improper tail

-doc "Builds the `map()` (any map) type.".
-spec map_type() -> c_types().
map_type()       -> #mapAnyTy{}.

-doc "Builds the `number()` type.".
-spec number_type() -> c_types().
number_type()       -> #builtTy{builtIn = number}.

-doc "Builds the `pid()` type.".
-spec pid_type() -> c_types().
pid_type()       -> #builtTy{builtIn = pid}.

-doc "Builds the `port()` type.".
-spec port_type() -> c_types().
port_type()       ->  #builtTy{builtIn = port}.

-doc "Builds the record type.".
-spec record_type() -> c_types().
record_type()       -> #recordFieldTy{}.

-doc "Builds the `reference()` type.".
-spec reference_type() -> c_types().
reference_type()       -> #builtTy{builtIn = reference}.

-doc "Builds the `tuple()` (any tuple) type.".
-spec tuple_type() -> c_types().
tuple_type()       -> #tupleAnyTy{}.

-doc "Builds a tuple type `{T1, ..., Tn}` from the given element types.".
-spec tuple_type(Args :: [c_types()]) -> c_types().
tuple_type(Args)       -> #tupleTy{args = Args}.

-doc """
Build a literal/singleton type from a concrete Erlang value.

Atoms, integers, floats and binaries become `#litTy{}` singletons;
lists and tuples become structural types over their elements; any
other term falls back to `dynamic()`.
""".
-spec build_lit(Types) -> c_types() when
      Types :: atom() | integer() | float() | binary() | list() | tuple() | dynamic().
build_lit(V)  when is_atom(V)    -> build_atom(V);
build_lit(V)  when is_integer(V) -> build_int(V);
build_lit(V)  when is_float(V)   -> build_float(V);
build_lit(V)  when is_binary(V)  -> build_bin(V);
build_lit([])                    -> build_list([]);
build_lit(V)  when is_list(V)    -> build_list(V);
build_lit(V)  when is_tuple(V)   -> build_tuple(V);
build_lit(_)                     -> dyn_type().

build_atom(V) when is_atom(V) ->
    #litTy{kind = 'atom', val = V}.

build_int(V) when is_integer(V) ->
    #litTy{kind = 'integer', val = V}.

build_float(V) when is_float(V) ->
    #litTy{kind = 'float', val = V}.

build_bin(V) when is_binary(V) ->
    #litTy{kind = 'binary', val = V}.

build_list([]) ->
    #emptyListTy{};
build_list(L) when is_list(L) ->
    list_of([build_lit(E) || E <- L]).

build_tuple(V) when is_tuple(V) ->
    #tupleTy{args=[build_lit(E) || E <- tuple_to_list(V)]}.

-doc "Returns the kind (`atom`, `integer`, `float`, `char`, `binary`) of a literal type.".
-spec lit_kind(Type :: c_types()) -> atom().
lit_kind(#litTy{kind = Kind}) -> Kind.

-doc """
Combine multiple function types (from multi-clause specs) into a single
function type with union args and union return type.

Example:
   fun((atom()) -> atom()) + fun((integer()) -> integer())
   => fun((atom() | integer()) -> atom() | integer())

 All function types must have the same arity.
""".
-spec combine_fun_types([funTy()]) -> funTy().
combine_fun_types([Single]) ->
    Single;
combine_fun_types([#funTy{anno = Anno, args = Args0, return = Ret0} | Rest]) ->
    Arity = length(Args0),
    lists:foldl(
      fun(#funTy{args = ArgsN, return = RetN}, Acc) ->
              Arity = length(ArgsN), %% assert same arity
              Acc#funTy{
                args = zip_union(Acc#funTy.args, ArgsN),
                return = make_union(Anno, [Acc#funTy.return, RetN])
               }
      end,
      #funTy{anno = Anno, args = Args0, return = Ret0},
      Rest).

-doc """
Extract the return type from a function type.

For `fun()` (`funAbsTy`, unknown signature) the return is `dynamic()`.
Returns `error` for non-function types.
""".
-spec fun_return(c_types()) -> c_types() | error.
fun_return(#funTy{return = Return}) ->
    Return;
fun_return(#funAnyTy{return = Return}) ->
    Return;
fun_return(#funAbsTy{}) ->
    dyn_type();
fun_return(_) ->
    error.

-doc """
Extract the argument types from a function type.

Returns `error` for `fun()` (`funAbsTy`, no known args) and for
non-function types.
""".
-spec fun_args(c_types()) -> [c_types()] | error.
fun_args(#funTy{args = Args}) ->
    Args;
fun_args(#funAnyTy{args = Args}) ->
    Args;
fun_args(_) ->
    error.

%% Pointwise union of argument lists.
-spec zip_union([c_types()], [c_types()]) -> [c_types()].
zip_union(As, Bs) ->
    lists:zipwith(fun(A, B) -> make_union(anno(A), [A, B]) end, As, Bs).

%% Build a union type, flattening nested unions and deduplicating.
-spec make_union(Anno, [c_types()]) -> c_types() when Anno :: non_neg_integer() | tuple().
make_union(_Anno, [Single]) ->
    Single;
make_union(Anno, Types) ->
    Flattened = flatten_union(Types),
    case lists:usort(Flattened) of
        [Single] -> Single;
        Many     -> #unionTy{anno = Anno, args = Many}
    end.

%% Flatten nested unionTy into a flat list.
-spec flatten_union([c_types()]) -> [c_types()].
flatten_union(Types) ->
    lists:flatmap(
      fun(#unionTy{args = Inner}) -> flatten_union(Inner);
         (Other)                  -> [Other]
      end, Types).

%% Extract annotation from any c_types() record.
-spec anno(c_types()) -> non_neg_integer() | tuple().
anno(#funTy{anno = A})         -> A;
anno(#varTy{anno = A})         -> A;
anno(#builtTy{anno = A})       -> A;
anno(#unionTy{anno = A})       -> A;
anno(#userTy{anno = A})        -> A;
anno(#remoteTy{anno = A})      -> A;
anno(#tupleTy{anno = A})       -> A;
anno(#tupleAnyTy{anno = A})    -> A;
anno(#recordTy{anno = A})      -> A;
anno(#mapTy{anno = A})         -> A;
anno(#mapAnyTy{anno = A})      -> A;
anno(#rangeTy{anno = A})       -> A;
anno(#opTy{anno = A})          -> A;
anno(#unOpTy{anno = A})        -> A;
anno(#bitString{anno = A})     -> A;
anno(#emptyListTy{anno = A})   -> A;
anno(#funAbsTy{anno = A})      -> A;
anno(#funAnyTy{anno = A})      -> A;
anno(#annTy{anno = A})         -> A;
anno(#boundedFunTy{anno = A})  -> A;
anno(#litTy{anno = A})         -> A;
anno(#recordFieldTy{anno = A}) -> A.


-doc """
Apply a substitution to a type, replacing every type variable
(`#varTy{}`) whose name is a key in the map with the corresponding
type. Variables not in the map are left unchanged.
""".
-spec apply_subst(#{atom() => c_types()}, c_types()) -> c_types().
apply_subst(Subst, #varTy{name = Name}) ->
    case maps:find(Name, Subst) of
        {ok, Replacement} -> Replacement;
        error             -> #varTy{name = Name}  %% free variable, leave as-is
    end;
apply_subst(Subst, #funTy{anno = Anno, args = Args, return = Ret}) ->
    #funTy{anno = Anno,
           args = [apply_subst(Subst, A) || A <- Args],
           return = apply_subst(Subst, Ret)};
apply_subst(Subst, #unionTy{anno = Anno, args = Args}) ->
    #unionTy{anno = Anno,
             args = [apply_subst(Subst, A) || A <- Args]};
apply_subst(Subst, #tupleTy{anno = Anno, args = Args}) ->
    #tupleTy{anno = Anno,
             args = [apply_subst(Subst, A) || A <- Args]};
apply_subst(Subst, #userTy{anno = Anno, name = Name, args = Args}) ->
    #userTy{anno = Anno, name = Name,
            args = [apply_subst(Subst, A) || A <- Args]};
apply_subst(Subst, #builtTy{anno = Anno, builtIn = B, args = Args}) ->
    #builtTy{anno = Anno, builtIn = B,
             args = [apply_subst(Subst, A) || A <- Args]};
apply_subst(Subst, #remoteTy{anno = Anno, mod = M, name = N, args = Args}) ->
    #remoteTy{anno = Anno, mod = M, name = N,
              args = [apply_subst(Subst, A) || A <- Args]};
apply_subst(Subst, #recordTy{anno = Anno, name = N, args = Fields}) ->
    #recordTy{anno = Anno, name = N,
              args = [apply_subst_record_field(Subst, F) || F <- Fields]};
apply_subst(Subst, #mapTy{anno = Anno, assocList = Assocs}) ->
    #mapTy{anno = Anno,
           assocList = [apply_subst_assoc(Subst, A) || A <- Assocs]};
apply_subst(Subst, #rangeTy{anno = Anno, lo = Lo, hi = Hi}) ->
    #rangeTy{anno = Anno,
             lo = apply_subst(Subst, Lo),
             hi = apply_subst(Subst, Hi)};
apply_subst(Subst, #annTy{anno = Anno, var = V, typ = T}) ->
    #annTy{anno = Anno, var = V, typ = apply_subst(Subst, T)};
apply_subst(Subst, #funAnyTy{anno = Anno, args = Args, return = Ret}) ->
    #funAnyTy{anno = Anno,
              args = [apply_subst(Subst, A) || A <- Args],
              return = apply_subst(Subst, Ret)};
apply_subst(Subst, #opTy{anno = Anno, op = Op, lhs = L, rhs = R}) ->
    #opTy{anno = Anno, op = Op,
          lhs = apply_subst(Subst, L),
          rhs = apply_subst(Subst, R)};
apply_subst(Subst, #unOpTy{anno = Anno, op = Op, arg = A}) ->
    #unOpTy{anno = Anno, op = Op, arg = apply_subst(Subst, A)};
%% Leaf types with no nested c_types() — return unchanged
apply_subst(_Subst, #emptyListTy{} = T)  -> T;
apply_subst(_Subst, #funAbsTy{} = T)     -> T;
apply_subst(_Subst, #mapAnyTy{} = T)     -> T;
apply_subst(_Subst, #tupleAnyTy{} = T)   -> T;
apply_subst(_Subst, #litTy{} = T)        -> T;
apply_subst(_Subst, #bitString{} = T)    -> T.

%%% ===================================================================
%%% Helpers for apply_subst on nested structures
%%% ===================================================================

apply_subst_assoc(Subst, #assocFieldTy{anno = Anno, key = K, val = V}) ->
    #assocFieldTy{anno = Anno,
                  key = apply_subst(Subst, K),
                  val = apply_subst(Subst, V)};
apply_subst_assoc(Subst, #assocExactTy{anno = Anno, key = K, val = V}) ->
    #assocExactTy{anno = Anno,
                  key = apply_subst(Subst, K),
                  val = apply_subst(Subst, V)}.

apply_subst_record_field(Subst, #recordFieldTy{anno = Anno, name = N, ty = T}) ->
      #recordFieldTy{anno = Anno, name = N, ty = apply_subst(Subst, T)}.


-doc """
Structural subtyping relation: `is_subtype(A, B)` is `true` when every
value of type `A` is also a value of type `B`.

`dynamic()` is compatible with any type in both directions (gradual
typing). Literals are subtypes of their general kind, `integer()` and
`float()` are subtypes of `number()`, lists are covariant in their
element type, tuples are pointwise, functions are contravariant in
arguments and covariant in the return, and unions distribute as usual.
""".
-spec is_subtype(c_types(), c_types()) -> boolean().

%% Dispatcher: types equal up to annotations are trivially subtypes
%% (reflexivity, annotation-insensitive); otherwise apply the
%% structural subtyping rules.
is_subtype(A, B) ->
    type_eq(A, B) orelse is_subtype_struct(A, B).

%% Annotation-insensitive structural equality: two types are type_eq
%% when they are identical after normalizing every annotation to 0.
-spec type_eq(c_types(), c_types()) -> boolean().
type_eq(A, B) ->
    strip(A) =:= strip(B).

%% Set every annotation to 0, recursively, so structurally equal types
%% that differ only in source location compare equal. One clause per
%% c_types() record. #litTy{}'s `val` is a plain value (not a type) and
%% is left untouched. #funConstraint{} carries no annotation.
-spec strip(c_types()) -> c_types().
strip(#annTy{typ = T} = X)       -> X#annTy{anno = 0, typ = strip(T)};
strip(#bitString{} = X)          -> X#bitString{anno = 0};
strip(#emptyListTy{} = X)        -> X#emptyListTy{anno = 0};
strip(#funAbsTy{} = X)           -> X#funAbsTy{anno = 0};
strip(#funAnyTy{args = As, return = R} = X) ->
    X#funAnyTy{anno = 0, args = strip_list(As), return = strip(R)};
strip(#rangeTy{lo = Lo, hi = Hi} = X) ->
    X#rangeTy{anno = 0, lo = strip(Lo), hi = strip(Hi)};
strip(#mapAnyTy{} = X)           -> X#mapAnyTy{anno = 0};
strip(#mapTy{assocList = As} = X) ->
    X#mapTy{anno = 0, assocList = [strip_assoc(A) || A <- As]};
strip(#opTy{lhs = L, rhs = R} = X) ->
    X#opTy{anno = 0, lhs = strip(L), rhs = strip(R)};
strip(#unOpTy{arg = A} = X)      -> X#unOpTy{anno = 0, arg = strip(A)};
strip(#builtTy{args = As} = X)   -> X#builtTy{anno = 0, args = strip_list(As)};
strip(#recordTy{args = Fs} = X)  -> X#recordTy{anno = 0, args = [strip_field(F) || F <- Fs]};
strip(#remoteTy{args = As} = X)  -> X#remoteTy{anno = 0, args = strip_list(As)};
strip(#tupleAnyTy{} = X)         -> X#tupleAnyTy{anno = 0};
strip(#tupleTy{args = As} = X)   -> X#tupleTy{anno = 0, args = strip_list(As)};
strip(#unionTy{args = As} = X)   -> X#unionTy{anno = 0, args = strip_list(As)};
strip(#varTy{} = X)              -> X#varTy{anno = 0};
strip(#userTy{args = As} = X)    -> X#userTy{anno = 0, args = strip_list(As)};
strip(#funTy{args = As, return = R} = X) ->
    X#funTy{anno = 0, args = strip_list(As), return = strip(R)};
strip(#boundedFunTy{funTy = F, constraint = Cs} = X) ->
    X#boundedFunTy{anno = 0, funTy = strip(F),
                   constraint = [strip_constraint(C) || C <- Cs]};
strip(#recordFieldTy{} = X)      -> strip_field(X);
strip(#litTy{} = X)              -> X#litTy{anno = 0}.

strip_list(L) -> [strip(T) || T <- L].

strip_assoc(#assocFieldTy{key = K, val = V} = X) ->
    X#assocFieldTy{anno = 0, key = strip(K), val = strip(V)};
strip_assoc(#assocExactTy{key = K, val = V} = X) ->
    X#assocExactTy{anno = 0, key = strip(K), val = strip(V)}.

strip_field(#recordFieldTy{ty = T} = X) ->
    X#recordFieldTy{anno = 0, ty = strip(T)}.

strip_constraint(#funConstraint{typeVar = V, ty = T} = X) ->
    X#funConstraint{typeVar = strip(V), ty = strip(T)}.

%% ---- structural subtyping rules (annotation-insensitive equality is
%% handled by the dispatcher above) --------------------------------

%% Top
is_subtype_struct(_, #builtTy{builtIn = 'any'})     -> true;
is_subtype_struct(_, #builtTy{builtIn = 'term'})    -> true;
is_subtype_struct(_, #builtTy{builtIn = 'dynamic'}) -> true;
is_subtype_struct(#builtTy{builtIn = 'dynamic'}, _) -> true;

%% Botttom
is_subtype_struct(#builtTy{builtIn = 'none'}, _) -> true;

%% Literal <: General
%% e.g. 1 <: integer()
is_subtype_struct(#litTy{kind = 'atom'}, #builtTy{builtIn = 'atom'})       -> true;
is_subtype_struct(#litTy{kind = 'integer'}, #builtTy{builtIn = 'integer'}) -> true;
is_subtype_struct(#litTy{kind = 'integer'}, #builtTy{builtIn = 'number'})  -> true;
is_subtype_struct(#litTy{kind = 'float'}, #builtTy{builtIn = 'float'})     -> true;
is_subtype_struct(#litTy{kind = 'float'}, #builtTy{builtIn = 'number'})    -> true;
is_subtype_struct(#litTy{kind = 'char'}, #builtTy{builtIn = 'char'})       -> true;
is_subtype_struct(#litTy{kind = 'char'}, #builtTy{builtIn = 'integer'})    -> true;
is_subtype_struct(#litTy{kind = 'char'}, #builtTy{builtIn = 'number'})     -> true;
is_subtype_struct(#litTy{kind = 'binary'}, #builtTy{builtIn = 'binary'})   -> true;

%% integer() <: number(), float() <: number()
is_subtype_struct(#builtTy{builtIn = 'integer'}, #builtTy{builtIn = 'number'}) -> true;
is_subtype_struct(#builtTy{builtIn = 'float'}, #builtTy{builtIn = 'number'})   -> true;

%% list subtyping
%% empty list is a subtype of any list and of itself
is_subtype_struct(#emptyListTy{}, #emptyListTy{}) -> true;
is_subtype_struct(#emptyListTy{}, #builtTy{builtIn = 'list'}) -> true;

%% any concrete list is a subtype of the "any list" (0-arg)
is_subtype_struct(#builtTy{builtIn = 'list', args = [_]},
                  #builtTy{builtIn = 'list', args = []}) -> true;

%% uniform list subtyping: covariant in the element type
is_subtype_struct(#builtTy{builtIn = 'list', args = [Ty0]},
                  #builtTy{builtIn = 'list', args = [Ty1]}) ->
    is_subtype(Ty0, Ty1);

%% tuple point-wise subtyping
is_subtype_struct(#tupleTy{}, #tupleAnyTy{}) -> true;
is_subtype_struct(#tupleTy{args=As}, #tupleTy{args=Bs})
  when length(As) =:= length(Bs) ->
    lists:all(fun({A, B}) -> is_subtype(A, B) end, lists:zip(As, Bs));

%% fun subtyping:
%% contravariant args: B <: A
%% covariant return: A <: B
is_subtype_struct(#funAnyTy{}, #funAbsTy{}) -> true;
is_subtype_struct(#funTy{}, #funAbsTy{}) -> true;
is_subtype_struct(#funTy{return=Ra}, #funAnyTy{return=Rb}) ->
    is_subtype(Ra, Rb);
is_subtype_struct(#funTy{args=As, return=Ra}, #funTy{args=Bs, return=Rb})
  when length(As) =:= length(Bs) ->
    lists:all(fun ({A, B}) -> is_subtype(B, A) end, lists:zip(As, Bs))
        andalso is_subtype(Ra, Rb);

%% map subtyping
is_subtype_struct(#mapTy{}, #mapAnyTy{}) -> true;
is_subtype_struct(#mapTy{assocList = As}, #mapTy{assocList = Bs}) ->
    %% Every field in B must be covered by some field in A
    lists:all(fun(B) -> lists:any(fun(A) -> is_subtype_assoc(A, B) end, As) end, Bs);

%% union subtyping
%% T <: T1 | T2 if T <: T1 or T <: T2
is_subtype_struct(T, #unionTy{args = Types}) ->
    lists:any(fun (Ty) -> is_subtype(T, Ty) end, Types);

%% Case: T1 | T2 <: T3 | T4 if T1 | T2 <: T3 or T1 | T2 <: T4 and
%% Case: T1 | T2 <: T3 if T1 <: T3 and T2 <: T3
is_subtype_struct(#unionTy{args=Types}, T) ->
    lists:all(fun(Ty) -> is_subtype(Ty, T) end, Types);

%% range
is_subtype_struct(#rangeTy{}, #builtTy{builtIn = 'integer'}) -> true;
is_subtype_struct(#rangeTy{}, #builtTy{builtIn = 'number'}) -> true;
is_subtype_struct(#litTy{kind = 'integer', val=V},
                  #rangeTy{lo = #litTy{val=L}, hi=#litTy{val=H}}) ->
    L =< V andalso V =< H;
is_subtype_struct(#rangeTy{lo = #litTy{val=La}, hi=#litTy{val=Ha}},
                  #rangeTy{lo = #litTy{val=Lb}, hi=#litTy{val=Hb}}) ->
    Lb =< La andalso Hb >= Ha;

%% catch-all
is_subtype_struct(_, _) -> false.


%% Exact satisfies exact
is_subtype_assoc(#assocExactTy{key = Ka, val = Va},
                 #assocExactTy{key = Kb, val = Vb}) ->
    is_subtype(Ka, Kb) andalso is_subtype(Va, Vb);
%% Exact satisfies optional (mandatory is stronger)
is_subtype_assoc(#assocExactTy{key = Ka, val = Va},
                 #assocFieldTy{key = Kb, val = Vb}) ->
    is_subtype(Ka, Kb) andalso is_subtype(Va, Vb);
%% Optional satisfies optional
is_subtype_assoc(#assocFieldTy{key = Ka, val = Va},
                 #assocFieldTy{key = Kb, val = Vb}) ->
    is_subtype(Ka, Kb) andalso is_subtype(Va, Vb);
%% Optional does NOT satisfy exact
is_subtype_assoc(#assocFieldTy{}, #assocExactTy{}) ->
      false.


-doc """
Greatest lower bound (intersection) of two types. Returns the more
precise of the two when one is a subtype of the other, otherwise
`none()` (the empty type) when they are incompatible.
""".
-spec meet(c_types(), c_types()) -> c_types().
meet(A, B) ->
    case is_subtype(A, B) of
        true ->
            A;
        false ->
            case is_subtype(B, A) of
                true ->
                    B;
                false ->
                    meet_distribute(A, B)
            end
    end.

%% Neither side is a subtype of the other. If one side is a union,
%% intersect member-wise and keep the members that survive: e.g.
%% meet(integer() | atom(), atom()) = atom(). This is required so a
%% type-test guard (is_atom) can narrow a union argument to the matching
%% member instead of collapsing to none(). When neither side is a union,
%% the types are genuinely disjoint and the meet is none().
meet_distribute(#unionTy{args = As}, B) ->
    meet_members(As, B);
meet_distribute(A, #unionTy{args = Bs}) ->
    meet_members(Bs, A);
meet_distribute(_A, _B) ->
    none_type().

%% Meet each union member with Other, dropping none() results, and
%% rebuild a union (or a single type, or none()) from the survivors.
meet_members(Members, Other) ->
    Survivors = [M2 || M <- Members,
                       M2 <- [meet(M, Other)],
                       not is_none(M2)],
    case Survivors of
        []      -> none_type();
        [Single] -> Single;
        Many    -> #unionTy{args = Many}
    end.


%%% ===================================================================
%%% Type Formatting
%%% ===================================================================

-doc """
Render a type back into human-readable Erlang type-spec syntax.

Returns an iolist suitable for `io:format("~s", [Iolist])` or
`unicode:characters_to_list/1`.
""".
-spec format_type(c_types()) -> iolist().
format_type(#litTy{kind = atom, val = V})    -> atom_to_list(V);
format_type(#litTy{kind = integer, val = V}) -> integer_to_list(V);
format_type(#litTy{kind = float, val = V})   -> float_to_list(V);
format_type(#litTy{kind = char, val = V})    -> [$$, V];
format_type(#litTy{kind = binary, val = V})  -> io_lib:format("~p", [V]);

format_type(#builtTy{builtIn = list, args = [T]}) ->
    ["[", format_type(T), "]"];
format_type(#builtTy{builtIn = B, args = []}) ->
    [atom_to_list(B), "()"];
format_type(#builtTy{builtIn = B, args = Args}) ->
    [atom_to_list(B), "(", format_args(Args), ")"];

format_type(#emptyListTy{}) -> "[]";
format_type(#tupleAnyTy{})  -> "tuple()";
format_type(#mapAnyTy{})    -> "map()";
format_type(#funAbsTy{})    -> "fun()";

format_type(#tupleTy{args = Args}) ->
    ["{", format_args(Args), "}"];

format_type(#unionTy{args = Args}) ->
    lists:join(" | ", [format_type(T) || T <- Args]);

format_type(#varTy{name = Name}) ->
    atom_to_list(Name);

format_type(#userTy{name = Name, args = Args}) ->
    [atom_to_list(Name), "(", format_args(Args), ")"];

format_type(#remoteTy{mod = M, name = N, args = Args}) ->
    [to_name(M), ":", to_name(N), "(", format_args(Args), ")"];

format_type(#rangeTy{lo = Lo, hi = Hi}) ->
    [format_type(Lo), "..", format_type(Hi)];

format_type(#funTy{args = Args, return = Ret}) ->
    ["fun((", format_args(Args), ") -> ", format_type(Ret), ")"];

format_type(#funAnyTy{return = Ret}) ->
    ["fun((...) -> ", format_type(Ret), ")"];

format_type(#recordTy{name = Name, args = Fields}) ->
    ["#", to_name(Name), "{", format_args(Fields), "}"];

format_type(#recordFieldTy{name = Name, ty = Ty}) ->
    [atom_to_list(Name), " :: ", format_type(Ty)];

format_type(#mapTy{assocList = Assocs}) ->
    ["#{", lists:join(", ", [format_assoc(A) || A <- Assocs]), "}"];

format_type(#bitString{bits = Bits, size = Size}) ->
    io_lib:format("<<_:~p, _:_*~p>>", [Bits, Size]);

format_type(#annTy{var = Var, typ = Ty}) ->
    [to_name(Var), " :: ", format_type(Ty)];

format_type(Other) ->
    %% Fallback for forms without a dedicated renderer (opTy, unOpTy,
    %% boundedFunTy, ...). Better a raw dump than a crash in the
    %% error-reporting path.
    io_lib:format("~p", [Other]).

format_args(Args) ->
    lists:join(", ", [format_type(T) || T <- Args]).

format_assoc(#assocFieldTy{key = K, val = V}) ->
    [format_type(K), " => ", format_type(V)];
format_assoc(#assocExactTy{key = K, val = V}) ->
    [format_type(K), " := ", format_type(V)].

to_name(A) when is_atom(A)   -> atom_to_list(A);
to_name(S) when is_list(S)   -> S;
to_name(B) when is_binary(B) -> B.
