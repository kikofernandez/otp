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



%% A :: T_0
-record(annTy, {anno = 0 :: non_neg_integer() | tuple(),
                var,
                typ :: c_types()}).

%% <<_:8, _:_*16>> 8 is bits, and 16 size
-record(bitString, {anno = 0 :: non_neg_integer() | tuple(),
                    bits = 0 :: integer(),
                    size = 0 :: integer()}).

-record(emptyListTy, {anno = 0 :: non_neg_integer() | tuple()}). % []
-record(funAbsTy, {anno = 0 :: non_neg_integer() | tuple()}).    % fun()
-record(funAnyTy, {anno = 0 :: non_neg_integer() | tuple(),
                   args     :: [c_types()],                      % {type,ANNO,any}
                   return   :: c_types()}).
-record(rangeTy, {anno = 0 :: non_neg_integer() | tuple(),
                  lo       :: c_types(),
                  hi       :: c_types()}).
-record(mapAnyTy, {anno = 0 :: non_neg_integer() | tuple()}).    % {type,ANNO,map,any}
-record(mapTy, {anno        :: non_neg_integer() | tuple(),
                assocList   :: [assocTy()]}).
-record(opTy, {anno = 0 :: non_neg_integer() | tuple(),
               rhs      :: c_types(),
               lhs      :: c_types(),
               op       :: atom()}).
-record(unOpTy, {anno = 0 :: non_neg_integer() | tuple(),
                 arg      :: c_types(),
                 op       :: atom()}).
-record(builtTy, {anno = 0 :: non_neg_integer() | tuple(),
                  builtIn  :: atom(),
                  args    = [] :: [c_types()]}). % [] represents builtIn type, e.g., tuple()
-record(recordTy, {anno = 0 :: non_neg_integer() | tuple(),
                   name     :: atom(),
                   args     :: [recordFieldTy()]}).
-record(remoteTy, {anno = 0 :: non_neg_integer() | tuple(),
                   mod      :: module(),
                   name     :: atom(),
                   args     :: [c_types()]}).
-record(tupleAnyTy, {anno = 0 :: non_neg_integer() | tuple()}).
-record(tupleTy, {anno = 0 :: non_neg_integer() | tuple(),
                  args     :: [c_types()]}).
-record(unionTy, {anno = 0 :: non_neg_integer() | tuple(),
                  args     :: [c_types()]}).
-record(varTy, {anno = 0 :: non_neg_integer() | tuple(),
                name     :: atom()}).
-record(userTy, {anno = 0 :: non_neg_integer() | tuple(),
                name      :: atom(),
                args      :: [c_types()]}).
-record(funTy, {anno = 0  :: non_neg_integer() | tuple(),
                args = [] :: [c_types()],
                return    :: c_types()}).
-record(boundedFunTy, {anno  = 0  :: non_neg_integer() | tuple(),
                       funTy      :: c_types(),
                       constraint :: [funConstraint()]}).
-record(funConstraint, { typeVar :: varTy(),
                         ty      :: c_types()}).
-record(assocFieldTy, {anno = 0 :: non_neg_integer() | tuple(),
                       key      :: c_types(),
                       val      :: c_types()}).
-record(assocExactTy, {anno = 0 :: non_neg_integer() | tuple(),
                       key      :: c_types(),
                       val      :: c_types()}).
-record(recordFieldTy, {anno = 0 :: non_neg_integer() | tuple(),
                        name = 0 :: atom(),
                        ty       :: c_types()}).
%% Literal / singleton type: the exact value 'ok', 42, $a, etc.
-record(litTy, {anno = 0 :: non_neg_integer() | tuple(),
                kind     :: atom | integer | float | char | binary,
                val      :: term()}).

-type annTy()          :: #annTy{}.
-type bitStringTy()    :: #bitString{}.
-type emptyListTy()    :: #emptyListTy{}.
-type funAbsTy()       :: #funAbsTy{}.
-type funAnyTy()       :: #funAnyTy{}.
-type rangeTy()        :: #rangeTy{}.
-type mapAnyTy()       :: #mapAnyTy{}.
-type mapTy()          :: #mapTy{}.
-type opTy()           :: #opTy{}.
-type unOpTy()         :: #unOpTy{}.
-type builtTy()        :: #builtTy{}.
-type recordTy()       :: #recordTy{}.
-type remoteTy()       :: #remoteTy{}.
-type tupleAnyTy()     :: #tupleAnyTy{}.
-type tupleTy()        :: #tupleTy{}.
-type unionTy()        :: #unionTy{}.
-type varTy()          :: #varTy{}.
-type userTy()         :: #userTy{}.
-type funTy()          :: #funTy{}.
-type boundedFunTy()   :: #boundedFunTy{}.
-type funConstraint()  :: #funConstraint{}.
-type assocFieldTy()   :: #assocFieldTy{}.
-type assocExactTy()   :: #assocExactTy{}.
-type recordFieldTy()  :: #recordFieldTy{}.
-type assocTy()        :: assocFieldTy() | assocExactTy().
-type litTy()          :: #litTy{}.

-type c_types()        :: annTy() | bitStringTy() | emptyListTy()
                        | funAbsTy() | funAnyTy() | funTy() | boundedFunTy()
                        | rangeTy() | mapAnyTy() | mapTy()
                        | opTy() | unOpTy() | builtTy()
                        | recordTy() | remoteTy() | litTy()
                        | tupleAnyTy() | tupleTy() | recordFieldTy()
                        | unionTy() | varTy() | userTy().


%%
%% Match on cerl:types()
%%
-define(ALIAS, alias).
-define(APPLY, apply).
-define(BINARY, binary).
-define(BITSTR, bitstr).
-define(CALL, call).
-define(CASE, 'case').
-define(CATCH, 'catch').
-define(CLAUSE, clause).
-define(CONS, cons).
-define(FUN, 'fun').
-define(LET, 'let').
-define(LETREC, letrec).
-define(LITERAL, literal).
-define(MAP, map).
-define(MAP_PAIR, map_pair).
-define(MOD, module).
-define(PRIMOP, primop).
-define(RECEIVE, 'receive').
-define(SEQ, seq).
-define(RECORD, record).
-define(RECORD_PAIR, record_pair).
-define(TRY, 'try').
-define(TUPLE, tuple).
-define(VALUES, values).
-define(VAR, var).
-define(OPAQUE, opaque).


%% Errors
-define(ERR_TYPE_ERROR(Reason), {type_error, Reason}).
-define(ERR_NOT_A_SUBTYPE(Got, Expected, Expr), {not_a_subtype, Got, Expected, Expr}).
