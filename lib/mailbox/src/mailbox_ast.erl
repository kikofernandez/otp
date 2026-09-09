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
%% Adapter module for Core Erlang AST operations.
%%
%% All interaction with `cerl` goes through this module.
%% This isolates the rest of the mailbox type checker from the
%% Core Erlang representation, making it possible to swap the
%% underlying IR without touching the checker or environment.
%%

-module(mailbox_ast).
-moduledoc false.

-type ast() :: cerl:cerl().
-type var_name() :: atom() | integer() | {atom(), arity()}.
-export_type([ast/0]).

%% Node type dispatch
-export([type/1]).

%% Pretty-printing
-export([format/1]).

%% Module-level
-export([module_attrs/1, module_defs/1]).

%% Function
-export([fun_body/1, fun_vars/1, is_fun/1]).

%% Case
-export([case_arg/1, case_clauses/1]).

%% Clause
-export([clause_body/1, clause_guard/1, clause_pats/1, clause_vars/1]).

%% Let
-export([let_arg/1, let_body/1, let_vars/1]).

%% Cons
-export([cons_hd/1, cons_tl/1]).

%% Try
-export([try_arg/1]).

%% Call
-export([call_module/1, call_name/1, call_args/1]).

%% Alias
-export([alias_pat/1, alias_var/1]).

%% Tuple
-export([tuple_es/1]).

%% Variable
-export([var_name/1]).

%% Literal / general
-export([concrete/1, is_literal/1]).

%% Pattern
-export([pat_vars/1]).

%% Primops
-export([primop_name/1]).

%% Apply
-export([apply_name/1, apply_args/1]).

%% Seq
-export([seq_arg/1, seq_body/1]).

%% Annotations
-export([get_ann/1]).

%% Values List
-export([values_es/1]).

%% Helpers
-export([free_vars/1]).

%%% ===================================================================
%%% Node type dispatch
%%% ===================================================================

-spec type(ast()) -> atom().
type(Node) ->
    cerl:type(Node).

%%% ===================================================================
%%% Pretty-printing
%%% ===================================================================

-spec format(ast()) -> string().
format(Node) ->
    cerl_prettypr:format(Node).

%%% ===================================================================
%%% Module-level
%%% ===================================================================

-spec module_attrs(ast()) -> [{ast(), ast()}].
module_attrs(Core) ->
    cerl:module_attrs(Core).

-spec module_defs(ast()) -> [{ast(), ast()}].
module_defs(Core) ->
    cerl:module_defs(Core).

%%% ===================================================================
%%% Function
%%% ===================================================================

-spec fun_body(ast()) -> ast().
fun_body(Fun) ->
    cerl:fun_body(Fun).

-spec fun_vars(ast()) -> [ast()].
fun_vars(Fun) ->
    cerl:fun_vars(Fun).

-spec is_fun(ast()) -> boolean().
is_fun(Node) ->
    cerl:is_c_fun(Node).

%%% ===================================================================
%%% Case
%%% ===================================================================

-spec case_arg(ast()) -> ast().
case_arg(Case) ->
    cerl:case_arg(Case).

-spec case_clauses(ast()) -> [ast()].
case_clauses(Case) ->
    cerl:case_clauses(Case).

%%% ===================================================================
%%% Cerl
%%% ===================================================================

-spec seq_arg(ast()) -> ast().
seq_arg(Seq) ->
    cerl:seq_arg(Seq).

-spec seq_body(ast()) -> ast().
seq_body(Seq) ->
    cerl:seq_body(Seq).

%%% ===================================================================
%%% Clause
%%% ===================================================================

-spec clause_body(ast()) -> ast().
clause_body(Clause) ->
    cerl:clause_body(Clause).

-spec clause_guard(ast()) -> ast().
clause_guard(Clause) ->
    cerl:clause_guard(Clause).

-spec clause_pats(ast()) -> [ast()].
clause_pats(Clause) ->
    cerl:clause_pats(Clause).

-spec clause_vars(ast()) -> [ast()].
clause_vars(Clause) ->
    cerl:clause_vars(Clause).

%%% ===================================================================
%%% Let
%%% ===================================================================

-spec let_arg(ast()) -> ast().
let_arg(Let) ->
    cerl:let_arg(Let).

-spec let_body(ast()) -> ast().
let_body(Let) ->
    cerl:let_body(Let).

-spec let_vars(ast()) -> [ast()].
let_vars(Let) ->
    cerl:let_vars(Let).

%%% ===================================================================
%%% Try
%%% ===================================================================

-spec try_arg(ast()) -> ast().
try_arg(Try) ->
    cerl:try_arg(Try).

%%% ===================================================================
%%% Call
%%% ===================================================================

-spec call_module(ast()) -> ast().
call_module(Call) ->
    cerl:call_module(Call).

-spec call_name(ast()) -> ast().
call_name(Call) ->
    cerl:call_name(Call).

-spec call_args(ast()) -> [ast()].
call_args(Call) ->
    cerl:call_args(Call).

%%% ===================================================================
%%% Alias
%%% ===================================================================

-spec alias_var(ast()) -> ast().
alias_var(Alias) ->
    cerl:alias_var(Alias).

-spec alias_pat(ast()) -> ast().
alias_pat(Alias) ->
    cerl:alias_pat(Alias).

%%% ===================================================================
%%% Tuple
%%% ===================================================================

-spec tuple_es(ast()) -> [ast()].
tuple_es(Tuple) ->
    cerl:tuple_es(Tuple).

%%% ===================================================================
%%% Variable
%%% ===================================================================

-spec var_name(ast()) -> var_name().
var_name(Var) ->
    cerl:var_name(Var).

%%% ===================================================================
%%% Literal / general
%%% ===================================================================

-spec concrete(ast()) -> term().
concrete(Node) ->
    cerl:concrete(Node).

-spec is_literal(ast()) -> boolean().
is_literal(Node) ->
    cerl:is_literal(Node).

%%% ===================================================================
%%% Pattern
%%% ===================================================================

-spec pat_vars(ast()) -> [ast()].
pat_vars(Pat) ->
    cerl:pat_vars(Pat).

%%% ===================================================================
%%% Cons
%%% ===================================================================

-spec cons_hd(ast()) -> ast().
cons_hd(Node) ->
    cerl:cons_hd(Node).


-spec cons_tl(ast()) -> ast().
cons_tl(Node) ->
    cerl:cons_tl(Node).

%%% ===================================================================
%%% Annotations
%%% ===================================================================

-spec get_ann(ast()) -> [term()].
get_ann(Node) ->
    cerl:get_ann(Node).

%%% ===================================================================
%%% Values List
%%% ===================================================================

-spec values_es(ast()) -> [ast()].
values_es(Node) ->
    cerl:values_es(Node).

%%% ===================================================================
%%% Primops
%%% ===================================================================

-spec primop_name(ast()) -> ast().
primop_name(Node) ->
    cerl:primop_name(Node).

%%% ===================================================================
%%% Application
%%% ===================================================================

-spec apply_name(ast()) -> ast().
apply_name(Node) ->
    cerl:apply_op(Node).

-spec apply_args(ast()) -> [ast()].
apply_args(Node) ->
    cerl:apply_args(Node).

%%% ===================================================================
%%% Helpers
%%% ===================================================================

-spec free_vars(ast()) -> [var_name()].
free_vars(Node) ->
    cerl_trees:free_variables(Node).
