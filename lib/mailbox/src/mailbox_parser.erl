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
%% Module that parses `erl_parse` types into an internal representation.
%%
%% Converts an `erl_parse` abstract format type to the mailbox internal
%% representation. This representation tries to streamline some differences,
%% such as eliminating bounded constraints in functions by replacing the
%% variables by their definitions. Because Erlang does not allow recursive
%% types, substitution terminates.
%%
%% Covers:
%%   - Types
%%   - Function Types
%%   - Function Constraints
%%   - Association Types
%%   - Record Fields
%%

-module(mailbox_parser).

-moduledoc false.

-export([from_abs_type/1]).

-include("c_types.hrl"). %% your header with record definitions


%%% ===================================================================
%%% Types
%%% ===================================================================
-spec from_abs_type(tuple()) -> c_types().
%% Annotated type: A :: T_0
from_abs_type({ann_type, Anno, [{var, _, Var}, T0]}) ->
    #annTy{anno = Anno, var = Var, typ = from_abs_type(T0)};

%% Atom literal
from_abs_type({atom, Anno, Val}) ->
    #litTy{anno = Anno, kind = atom, val = Val};

%% Integer literal
from_abs_type({integer, Anno, Val}) ->
    #litTy{anno = Anno, kind = integer, val = Val};

%% Character literal
from_abs_type({char, Anno, Val}) ->
    #litTy{anno = Anno, kind = char, val = Val};

%% Bitstring type: <<_:M, _:_*N>>
from_abs_type({type, Anno, binary, [M, N]}) ->
    #bitString{anno = Anno,
               bits = integer_val(M),
               size = integer_val(N)};

%% Empty list type: []
from_abs_type({type, Anno, nil, []}) ->
    #emptyListTy{anno = Anno};

%% Fun type: fun()
from_abs_type({type, Anno, 'fun', []}) ->
    #funAbsTy{anno = Anno};

%% Fun type: fun((...) -> T_0)
from_abs_type({type, Anno, 'fun', [{type, _, any}, T0]}) ->
    #funAnyTy{anno = Anno, args = [], return = from_abs_type(T0)};

%% Fun type: fun((T_1, ..., T_n) -> T_0) — product form
from_abs_type({type, Anno, 'fun',
               [{type, _, product, Args}, T0]}) ->
    #funTy{anno = Anno,
           args = [from_abs_type(A) || A <- Args],
           return = from_abs_type(T0)};

%% Constrained function type: Ft when Fc
%% Bounded function types are expanded inline: type variable constraints
%% are substituted into the function args and return type, eliminating
%% the bounded_fun wrapper entirely.
%%
%% Erlang does not allow recursive types in bounded funs, so this
%% substitution always terminates.
from_abs_type({type, _Anno, bounded_fun, [Ft, Constraints]}) ->
      %% 1. Build substitution map from constraints: Var => Type
      Subst = build_subst(Constraints),
      %% 2. Convert the inner function type
      FunTy = from_abs_type(Ft),
      %% 3. Apply substitution to expand all type variables
      mailbox_types:apply_subst(Subst, FunTy);

%% Integer range type: L .. H
from_abs_type({type, Anno, range, [L, H]}) ->
    #rangeTy{anno = Anno,
             lo = from_abs_type(L),
             hi = from_abs_type(H)};

%% Map type: map() (any map)
from_abs_type({type, Anno, map, any}) ->
    #mapAnyTy{anno = Anno};

%% Map type: #{A_1, ..., A_k}
from_abs_type({type, Anno, map, Assocs}) when is_list(Assocs) ->
    #mapTy{anno = Anno,
           assocList = [from_abs_assoc(A) || A <- Assocs]};

%% Binary operator type: T_1 Op T_2
from_abs_type({op, Anno, Op, Lhs, Rhs}) ->
    #opTy{anno = Anno,
          op = Op,
          lhs = from_abs_type(Lhs),
          rhs = from_abs_type(Rhs)};

%% Unary operator type: Op T_0
from_abs_type({op, Anno, Op, Arg}) ->
    #unOpTy{anno = Anno, op = Op, arg = from_abs_type(Arg)};

%% Record type: #Name{F_1, ..., F_k}
from_abs_type({type, Anno, record, [{atom, _, Name} | Fields]}) ->
    #recordTy{anno = Anno,
              name = Name,
              args = [from_abs_record_field(F) || F <- Fields]};

%% Remote type: M:N(T_1, ..., T_k)
from_abs_type({remote_type, Anno, [{atom, _, Mod}, {atom, _, Name}, Args]}) ->
    #remoteTy{anno = Anno,
              mod = Mod,
              name = Name,
              args = [from_abs_type(A) || A <- Args]};

%% Tuple type: tuple() (any tuple)
from_abs_type({type, Anno, tuple, any}) ->
    #tupleAnyTy{anno = Anno};

%% Tuple type: {T_1, ..., T_k}
from_abs_type({type, Anno, tuple, Args}) when is_list(Args) ->
    #tupleTy{anno = Anno,
             args = [from_abs_type(A) || A <- Args]};

%% Union type: T_1 | ... | T_k
from_abs_type({type, Anno, union, Args}) ->
    #unionTy{anno = Anno,
             args = [from_abs_type(A) || A <- Args]};

%% Type variable: V
from_abs_type({var, Anno, Name}) ->
    #varTy{anno = Anno, name = Name};

%% User-defined type: N(T_1, ..., T_k)
from_abs_type({user_type, Anno, Name, Args}) ->
    #userTy{anno = Anno,
            name = Name,
            args = [from_abs_type(A) || A <- Args]};

%% Predefined (built-in) type: N(T_1, ..., T_k)
%% This is the catch-all for {type, Anno, Name, Args} not matched above.
from_abs_type({type, Anno, Name, Args}) when is_atom(Name), is_list(Args) ->
    #builtTy{anno = Anno,
             builtIn = Name,
             args = [from_abs_type(A) || A <- Args]}.

%%% ===================================================================
%%% Association Types
%%% ===================================================================

%% K => V (optional association)
from_abs_assoc({type, Anno, map_field_assoc, [K, V]}) ->
    #assocFieldTy{anno = Anno,
                  key = from_abs_type(K),
                  val = from_abs_type(V)};

%% K := V (mandatory/exact association)
from_abs_assoc({type, Anno, map_field_exact, [K, V]}) ->
    #assocExactTy{anno = Anno,
                  key = from_abs_type(K),
                  val = from_abs_type(V)}.

%%% ===================================================================
%%% Record Field Types
%%% ===================================================================

%% Name :: Type
from_abs_record_field({type, Anno, field_type, [{atom, _, Name}, T]}) ->
    #recordFieldTy{anno = Anno,
                   name = Name,
                   ty = from_abs_type(T)}.

%%% ===================================================================
%%% Helpers
%%% ===================================================================

%% Extract integer value from a singleton integer type representation.
integer_val({integer, _, V}) -> V;
integer_val({op, _, '-', {integer, _, V}}) -> -V.

%%% ===================================================================
%%% Substitution for bounded fun expansion
%%% ===================================================================

%% Build a substitution map from constraint list.
%% Each constraint is: {type, _, constraint, [{atom, _, is_subtype}, [{var, _, V}, T]]}
%% We convert the type T and map variable name V to it.
-spec build_subst(list()) -> #{atom() => c_types()}.
build_subst(Constraints) ->
      maps:from_list(
        [{Var, from_abs_type(T)}
         || {type, _, constraint,
             [{atom, _, is_subtype}, [{var, _, Var}, T]]} <:- Constraints]).
