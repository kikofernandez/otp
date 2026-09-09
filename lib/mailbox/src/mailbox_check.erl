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
%% Module that performs mailbox typing.
%%
%% This module performs most of the heavy lifting of the mailbox typing.
%% It includes passes such as the pretyping, mailbox typing, and constraint gen.
%%
-module(mailbox_check).
-moduledoc false.

-export([check_module/3, format_error/1]).

-include("c_types.hrl").

-spec check_module(Env, Defs, Opts) -> ok | {error, list()} when
      Env :: mailbox_env:env(),
      Defs :: [{Name :: mailbox_ast:ast(), Def :: mailbox_ast:ast()}],
      Opts :: list().
check_module(Env, Defs, _Opts) ->
    Errors =
        lists:filtermap(
          fun ({_Name, Def}) ->
                  LocalEnv = mailbox_env:build_fun_env(Def, Env),
                  try check_fun(Env, LocalEnv, Def) of
                      ok -> false;
                      {error, Reason} -> {true, Reason}
                  catch
                      throw:?ERR_TYPE_ERROR(Reason) -> {true, Reason}
                  end
          end, Defs),
    case Errors of
        [] ->
            ok;
        _ ->
            {error, Errors}
    end.

%% Entry point to type checker
check_fun(Env, LocalEnv, Def) ->
    case mailbox_env:fun_type(Def, Env) of
        {ok, Type}  ->
            check(Env, LocalEnv, Def, Type);
        error ->
            %% assume dynamic
            Vars = mailbox_ast:fun_vars(Def),
            Ret = mailbox_types:dyn_type(),
            Args = [mailbox_types:dyn_type() || _ <- Vars],
            FunType = mailbox_types:fun_type(Args, Ret),
            check(Env, LocalEnv, Def, FunType)
    end.

%% dispatch method
-spec check(Env, LocalEnv, Node, Ty) -> Return when
      Env :: mailbox_env:env(),
      LocalEnv :: mailbox_env:local_env(),
      Node :: mailbox_ast:ast(),
      Ty :: c_types(),
      Return :: ok | {error, term()}.
check(Env, LocalEnv, Expr, Ty) ->
    case mailbox_ast:type(Expr) of
        ?FUN ->
            check_fun(Env, LocalEnv, Expr, Ty);
        ?CASE ->
            check_case(Env, LocalEnv, Expr, Ty);
        ?LITERAL ->
            check_literal(Env, LocalEnv, Expr, Ty);
        ?ALIAS ->
            check_alias(Env, LocalEnv, Expr, Ty);
        %% ?TUPLE ->
        %%     check_tuple(Env, LocalEnv, Expr, Ty);
        ?VAR ->
            check_var(Env, LocalEnv, Expr, Ty) ;
        ?PRIMOP ->
            check_primop(Env, LocalEnv, Expr, Ty);
        ?APPLY ->
            check_apply(Env, LocalEnv, Expr, Ty);
        ?CALL ->
            check_call(Env, LocalEnv, Expr, Ty);
        ?SEQ ->
            check_seq(Env, LocalEnv, Expr, Ty)
    end.


%% NOTE: We are assuming we have the type specs and this
%%       rule is used when typing an exported function.
%%
%%       For functions without specs, we crash and burn at the moment
%%
%%        G, x: A1 |- e <= A2
%%    --------------------------------------
%%        G |- fun (x. e) <= A1 -> A2
%%
check_fun(Env, LocalEnv0, Fun, Ty) ->
    LocalEnv1 = mailbox_env:build_fun_env(Fun, Env),
    LocalEnv = mailbox_env:merge_env(LocalEnv0, LocalEnv1),

    ReturnTy = mailbox_types:fun_return(Ty),
    Body = mailbox_ast:fun_body(Fun),
    check(Env, LocalEnv, Body, ReturnTy).

%%
%% Apply does not exist in checking form, so we infer.
%%
%%     G |- m:e => B      B <: A
%%  ---------------------------------
%%       G |- m:e <= A
%%
check_call(_Env, _LocalEnv, Node, Ty) ->
    subsumption(_Env, _LocalEnv, Node, Ty).



%%
%%     G |- e1 => B      G |- e2 <= A
%%  ------------------------------------
%%       G |- e1; e2 <= A
%%
check_seq(_Env, _LocalEnv, Node, Ty) ->
     _ = synth(_Env, _LocalEnv, mailbox_ast:seq_arg(Node)),
    check(_Env, _LocalEnv, mailbox_ast:seq_body(Node), Ty).

%%
%% Apply does not exist in checking form, so we infer.
%%
%%     G |- e => B      B <: A
%%  ---------------------------------
%%       G |- e <= A
%%
check_apply(_Env, _LocalEnv, Node, Ty) ->
    subsumption(_Env, _LocalEnv, Node, Ty).

subsumption(_Env, _LocalEnv, Node, Ty) ->
    SynthTy = synth(_Env, _LocalEnv, Node),
    check_subtype(SynthTy, Ty, Node).

check_alias(_Env, _LocalEnv, Node, Ty) ->
    check(_Env, _LocalEnv, mailbox_ast:alias_pat(Node), Ty).

%%
%%  Example rule below, and the same for all literal types
%%
%%  --------------------------
%%      G |- lit <= type()
%%
check_literal(_Env, _LocalEnv, Lit, Ty) ->
    LitTy = mailbox_types:build_lit(mailbox_ast:concrete(Lit)),
    check_subtype(LitTy, Ty, Lit).

%%
%%   --------------------------------------
%%          G |- primop e <= A
%%
check_primop(_Env, _LocalEnv, _Node, _Ty) ->
    %% Trivially satisfied for raise, build_stacktrace, nif_start, etc.
    %% TODO: needs to handle receives
    ok.

%%
%%        G |- x => A      A <: B
%%   -------------------------------------
%%       G |- x <= B
%%
check_var(Env, LocalEnv, Node, TyB) ->
    TyA = synth(Env, LocalEnv, Node),
    check_subtype(TyA, TyB, Node).

check_subtype(TyA, TyB, Node) ->
    case mailbox_types:is_subtype(TyA, TyB) of
        true ->
            ok;
        false ->
            {error, ?ERR_NOT_A_SUBTYPE(TyA, TyB, Node)}
    end.

%% TODO:
%% check_tuple(_Env, _LocalEnv, _Tuple, _Ty) ->
%%     error(check_tuple).

%% NOTE:
%% This is not the rule from Bidirectional Typing from Jana Dunfield and Neel Krishnaswami.
%% The rule we have is:
%%
%%                          G, x1:A1+A2 |- e1 <= B
%% G |- e => A1 + A2        G, x2:A1+A2 |- e12 <= B
%% -----------------------------------------------------
%%     G |- case (e, inj1 x2. e1, inj2 x2. e2) <= B
%%
%%
%% It is an adaptation and needs narrowing of types based on the guards of each branch, if any.
%%
%%   case e of
%%      {X, Y} when ... -> e1;
%%      ok              -> e2;
%%      Z               -> e3
%%   end
%%
%% At the moment, the example type checks, because the pattern has enough structure,
%% but the union type is pushed as follows:
%%
%% Assume e :: T1 + T2 + T3, a union type with three types, one per branch. Then, based
%% on the modified rule above, we have that
%%
%%    {X, Y} :: T1 + T2 + T3,
%%    ok     :: T1 + T2 + T3
%%    Z      :: T1 + T2 + T3
%%
%% This means that we have not narrowed down the types.
%% The implication is that if we perform `is_tuple({X, Y})`, the type checker would fail
%% because `{X, Y} :: T1 + T2 + T3`, but is_tuple :: tuple() and T1 + T2 + T3 <: tuple() is not satisfied.
%%
%%
check_case(Env, LocalEnv, Case, Ty) ->
    Arg = mailbox_ast:case_arg(Case),
    ArgTy = synth(Env, LocalEnv, Arg),

    Clauses = mailbox_ast:case_clauses(Case),
    check_clauses(Env, LocalEnv, ArgTy, Clauses, Ty).


%% TODO: This is a fail-fast error.
%% We stop typechecking and return the first error we encounter.
check_clauses(Env, LocalEnv, ArgTy, Clauses, RetTy) ->
    lists:foldl(
      fun(_C, {error, _} = Err) ->
              Err;
         (C, ok) ->
              check_clause(Env, LocalEnv, ArgTy, C, RetTy)
      end, ok, Clauses).


%%
%%        G |- g => {v : Va | v in vars(g) }
%%        G, {v : Va | v in vars(g) } |- p <= A
%% --------------------------------------------------------
%%             G |- p when g <= A
%%
check_clause(Env, LocalEnv, PatTy, C, RetTy) ->
    %% PatTy: Type of the pattern
    %% RetTy: Return type of the expression body of the pattern
    Body = mailbox_ast:clause_body(C),
    case is_match_fail(Body) of
        true ->
            %% Compiler-generated catch-all (function_clause/case_clause).
            %% Not user code — nothing to type-check.
            ok;
        false ->
            GuardEnv = synth_env_guards(LocalEnv, C),
            PatEnv = synth_env_patterns(Env, LocalEnv, GuardEnv, PatTy, C),
            io:format("[~p] ~p~nGuards:~n~p~nPattern~n~p~n", [?LINE, ?FUNCTION_NAME, GuardEnv, PatEnv]),
            check(Env, PatEnv, Body, RetTy)
    end.

%% A clause body of `primop 'match_fail'(...)` is the compiler-inserted
%% catch-all that raises function_clause/case_clause. It is desugaring
%% noise, not user code.
is_match_fail(Body) ->
    case mailbox_ast:type(Body) of
        ?PRIMOP ->
            'match_fail' =:= mailbox_ast:concrete(mailbox_ast:primop_name(Body));
        _ ->
            false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Synthesize Type
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Synthesizing mode of bidirectional type checkers.
%% Reference. Bidirectional Typing by Jana Dunfield and Neel K.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec synth(Env :: mailbox_env:env(), LocalEnv :: mailbox_env:local_env(), Arg :: mailbox_ast:ast()) -> c_types().
synth(Env, LocalEnv, Arg) ->
    case mailbox_ast:type(Arg) of
        ?VAR ->
            synth_var(Env, LocalEnv, Arg);
        ?ALIAS ->
            synth_alias(Env, LocalEnv, Arg);
        ?TUPLE ->
            synth_tuple(Env, LocalEnv, Arg);
        ?LITERAL ->
            synth_literal(Env, LocalEnv, Arg);
        ?CONS ->
            synth_cons(Env, LocalEnv, Arg);
        ?APPLY ->
            synth_apply(Env, LocalEnv, Arg);
        ?CALL ->
            synth_call(Env, LocalEnv, Arg);
        ?CASE ->
            synth_case(Env, LocalEnv, Arg)
        %% ?VALUES ->
        %%     synth_values(Env, LocalEnv, Arg)
    end.

%%
%%        G |- e1 => A
%%        G |- e2 => B
%%        G |- e3 => C
%%  ----------------------------------------
%%        G |- <e1,e2,e3> => [A, B, C]
%%
%% synth_values(_Env, _LocalEnv, Arg) ->
%%     mailbox_ast:values_es(Arg),
%%     %% io:format("[~p]~p~n~p~n", [?LINE, ?FUNCTION_NAME, Arg]),
%%     error(error_in_values).

%% Works only on top level-functions with specs.
%% TODO: lambdas are not covered:
%%       - it needs Consistent Subtyping for all
%%       - contextual typing from
%% TODO: un-spec functions
%%
%%     G |- e1 => A -> B
%%     G |- e2 <= A
%%   -----------------------
%%       G |- e1 e2 => B
%%
synth_apply(Env, LocalEnv, Arg) ->
    Name = mailbox_ast:apply_name(Arg),
    Args = mailbox_ast:apply_args(Arg),
    SynthType = synth(Env, LocalEnv, Name),
    case mailbox_types:fun_args(SynthType) of
        ArgTypes when is_list(ArgTypes),
                      length(ArgTypes) =:= length(Args) ->
            check_args(Env, LocalEnv, Args, ArgTypes),
            mailbox_types:fun_return(SynthType);
        _ ->
            %% arity mismatch or non-function operator: stay permissive
            mailbox_types:dyn_type()
    end.

%% Check argument expressions against expected types. Fail-fast:
%% raises via fail/1 on the first mismatch, so argument type errors
%% propagate to the check_module boundary rather than being dropped.
check_args(Env, LocalEnv, Args, ArgTypes) ->
    lists:foreach(
      fun({A, T}) ->
              case check(Env, LocalEnv, A, T) of
                  ok -> ok;
                  {error, Reason} -> fail(Reason)
              end
      end, lists:zip(Args, ArgTypes)).

%%
%%     G |- m:e1 => A -> B      G |- e2 <= A
%%   -----------------------------------------
%%              G |- m:e1 e2 => B
%%
%% Remote targets are required to have a spec (user-declared or
%% TypER-synthesized and loaded into the module DB). If none is found,
%% we fall back to dynamic() to stay sound-but-permissive (#13.5).
%%
synth_call(Env, LocalEnv, Node) ->
    Mod  = mailbox_ast:concrete(mailbox_ast:call_module(Node)),
    Name = mailbox_ast:concrete(mailbox_ast:call_name(Node)),
    Args = mailbox_ast:call_args(Node),
    case mailbox_env:lookup_modules({Mod, Name, length(Args)}, Env) of
        {ok, Type} ->
            case mailbox_types:fun_args(Type) of
                ArgTypes when is_list(ArgTypes),
                              length(ArgTypes) =:= length(Args) ->
                    check_args(Env, LocalEnv, Args, ArgTypes),
                    mailbox_types:fun_return(Type);
                _ ->
                    %% arity mismatch or non-function spec
                    mailbox_types:dyn_type()
            end;
        error ->
            mailbox_types:dyn_type()
    end.



%% TODO:
%%
%%
%%
%%       G |- e => A1 + A2 + A3
%%       G, p1: A1 + A2 + A3 |- e1 => A
%%       G, p2: A1 + A2 + A3 |- e2 => A
%%  ---------------------------------------------------
%%       G |- case e of p1 when g -> e1, ... end => A
%%
synth_case(Env, LocalEnv, Case) ->
    Arg = mailbox_ast:case_arg(Case),
    ArgTy = synth(Env, LocalEnv, Arg),

    Clauses = mailbox_ast:case_clauses(Case),
    synth_clauses(Env, LocalEnv, ArgTy, Clauses).

%% TODO: This is a fail-fast error.
%% We stop typechecking and return the first error we encounter.
synth_clauses(Env, LocalEnv, ArgTy, Clauses) ->
    Types =
        lists:foldl(
          fun(_C, {error, _} = Err) ->
                  %% TODO: not possible?
                  Err;
             (C, Acc) ->
                  [synth_clause(Env, LocalEnv, ArgTy, C) | Acc]
          end, [], Clauses),
    mailbox_types:union(Types).

%%
%%        G |- g => {v : Va | v in vars(g) }
%%        G, {v : Va | v in vars(g) } |- p => A
%% --------------------------------------------------------
%%             G |- p when g => A
%%
synth_clause(Env, LocalEnv, PatTy, C) ->
    %% PatTy: Type of the pattern
    Body = mailbox_ast:clause_body(C),
    case is_match_fail(Body) of
        true ->
            %% Compiler-generated catch-all (function_clause/case_clause).
            %% Not user code — nothing to type-check.
            ok;
        false ->
            GuardEnv = synth_env_guards(LocalEnv, C),
            PatEnv = synth_env_patterns(Env, LocalEnv, GuardEnv, PatTy, C),
            synth(Env, PatEnv, Body)
    end.

%%
%%        (x : A) in G
%%   ------------------------
%%         G |- x => A
%%
synth_var(_Env, LocalEnv, Arg) ->
    case mailbox_ast:var_name(Arg) of
        {_Fname, Arity}=FA ->
            case mailbox_env:lookup_spec(FA, _Env) of
                {ok, Typ} ->
                    Typ;
                error ->
                    Args = [mailbox_types:dyn_type()
                            || _ <- lists:seq(1, Arity) ],
                    Ret = mailbox_types:dyn_type(),
                    mailbox_types:fun_type(Args, Ret)
            end;
        VarName ->
            case mailbox_env:get_var(VarName, LocalEnv) of
                {ok, Typ} -> Typ;
                error -> mailbox_types:dyn_type()
            end
    end.


%%  Note: This may not be the correct rule and we could do
%%  as Annette Bieniusa in Erlang'21 paper and make sure that
%%  A == B. I do not want to impose this restriction just yet.
%%
%%           G |- e1 => A
%%           G |- e2 => B
%%    -------------------------------------
%%           G |- [e1 | e2] => [A, B]
%%
synth_cons(_Env, LocalEnv, Arg) ->
    Hd = mailbox_ast:cons_hd(Arg),
    Tl = mailbox_ast:cons_tl(Arg),
    SynthEnv = synth_env(_Env, LocalEnv, Arg),
    NewEnv = mailbox_env:merge_env_meet(SynthEnv, LocalEnv),
    HTy = synth(_Env, NewEnv, Hd),
    TTy = synth(_Env, NewEnv, Tl),
    mailbox_types:cons(HTy, TTy).

synth_alias(_Env, LocalEnv, Arg) ->
    synth(_Env, LocalEnv, mailbox_ast:alias_pat(Arg)).

%%
%%                       G |- x0 => A0
%%                       G |- x1 => A1
%%                       ...
%%                       G |- xn => An
%%   -----------------------------------------------------
%%         G |- {x0, x1, ...xn} => {A0, A1, ..., An}
%%
synth_tuple(_Env, LocalEnv, Arg) ->
    ItemsInTuple = mailbox_ast:tuple_es(Arg),
    mailbox_types:tuple_type([synth(_Env, LocalEnv, I) || I <- ItemsInTuple]).

%%
%%  From the shape we can generate the type of a literal
%%
%%   ------------------------------
%%           G |- lit => type()
%%
synth_literal(_Env, _LocalEnv, Arg) ->
    mailbox_types:build_lit(mailbox_ast:concrete(Arg)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Synthesize Environment
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This is useful for patterns and guards, who create new bindings.
%% Pretty much, any operation in Erlang can introduce a new binding.
%% e.g.,
%%
%% 1. {Y, Z}=X = {a, b} introduces new bindings on assignment
%% 2. case e of p when g introduces bindings in p
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

synth_env(Env, LocalEnv, Arg) ->
    case mailbox_ast:type(Arg) of
        ?VAR ->
            synth_env_var(Env, LocalEnv, Arg);
        ?ALIAS ->
            synth_env_alias(Env, LocalEnv, Arg);
        ?TUPLE ->
            synth_env_tuple(Env, LocalEnv, Arg);
        ?LITERAL ->
            synth_env_literal(Env, LocalEnv, Arg);
        ?CONS ->
            synth_env_cons(Env, LocalEnv, Arg)
    end.

synth_env_alias(_Env, LocalEnv, Arg) ->
    Name = mailbox_ast:var_name(mailbox_ast:alias_var(Arg)),
    Ty = synth(_Env, LocalEnv, mailbox_ast:alias_pat(Arg)),
    Pat = mailbox_ast:alias_pat(Arg),
    SynthEnv = synth_bindings(_Env, LocalEnv, Pat),
    mailbox_env:put_var(Name, Ty, mailbox_env:merge_env_meet(LocalEnv, SynthEnv)).

%%
%%       G |- e1 -| D1
%%       G |- e2 -| D2
%%       ...
%%       G |- e -| Dn
%%       meet(D0, ..., Dn, G) = D
%%   ----------------------------------
%%       G |- {e1, e2, ..., en} -| D
%%
synth_env_tuple(_Env, LocalEnv, Arg) ->
    case mailbox_ast:free_vars(Arg) of
        [_|_] ->
            ItemsInTuple = mailbox_ast:tuple_es(Arg),
            mailbox_env:fold(
              fun (Node, Acc) ->
                      SynthEnv = synth_bindings(_Env, LocalEnv, Node),
                      mailbox_env:merge_env_meet(SynthEnv, Acc)
              end, LocalEnv, ItemsInTuple);
        [] ->
            mailbox_env:new_localenv()
    end.

%%
%%       [H | T] = L
%%       G |- H -| D0
%%       G |- T -| D1
%%       meet(D0, D1) = D variable-wise
%%   ----------------------------------
%%       G |- L -| D
%%
synth_env_cons(_Env, LocalEnv, Arg) ->
    case mailbox_ast:free_vars(Arg) of
        [_|_] ->
            Head = mailbox_ast:cons_hd(Arg),
            Tail = mailbox_ast:cons_tl(Arg),

            SynthEnvHd = synth_bindings(_Env, LocalEnv, Head),
            SynthEnvTl = synth_bindings(_Env, LocalEnv, Tail),
            SynthEnv = mailbox_env:merge_env_meet(SynthEnvHd, SynthEnvTl),
            mailbox_env:merge_env_meet(SynthEnv, LocalEnv);
        [] ->
            mailbox_env:new_localenv()
    end.

%%
%%   ----------------------------------
%%       G |- x -| x : dyn()
%%
synth_env_var(_Env, LocalEnv, Node) ->
    Name = mailbox_ast:var_name(Node),
    NewEnv = mailbox_env:put_var(Name,
                                mailbox_types:dyn_type(),
                                mailbox_env:new_localenv()),
    mailbox_env:merge_env_meet(LocalEnv, NewEnv).

%%
%%   ----------------------------------
%%       G |- lit() -| empty_env()
%%
synth_env_literal(_Env, _LocalEnv, _Arg) ->
    mailbox_env:new_localenv().


%%
%%  Note: Guards have been dealt with elsewhere
%%
%%         [G |- pi when true -| Di : for all p0, g0]
%%         mergeEnv(Di) = D
%%   -------------------------------------------------
%%       G |- p0 when true; p1 when true -| D
%%
synth_env_patterns(Env, LocalEnv, GuardEnv, PatTy, C) ->
    Pats = mailbox_ast:clause_pats(C),
    PatEnv1 = [synth_env_pattern(Env, LocalEnv, GuardEnv, PatTy, Pat) || Pat <- Pats],
    lists:foldl(fun mailbox_env:merge_env_meet/2, LocalEnv, PatEnv1).

%%       G |- p when true -| D
%%       G |- p => B             B <: A
%%   -------------------------------------------------
%%       G |- p when true => A -| D
synth_env_pattern(Env, _LocalEnv, GuardEnv, PatTy, Pat) ->
    %% Synth return type of the pattern. e.g.,
    %%
    %% {a=Y, b}=X when is_atom(X)
    %%
    %% Synthesizes type {a, b}
    %%
    PatSynthType = synth(Env, GuardEnv, Pat),

    %% Checks that the checked type is a super-type of the synth type.
    case mailbox_types:is_subtype(PatSynthType, PatTy) of
        true  -> ok;
        false -> fail(?ERR_NOT_A_SUBTYPE(PatSynthType, PatTy, Pat))
    end,

    %% Guards and Patterns synth an environment, as they have enough
    %% type information to generate bindings. e.g.,
    %%
    %% {a=Y, b}=X when is_atom(X)
    %%
    %% Generates Bindings G ::= X :: {a, b}, Y :: a
    %% We use type precision (meet) to refine that X is an atom
    %% but X :: {a, b} is more precise than X :: atom()
    %%
    synth_env(Env, GuardEnv, Pat).

%% Note. This is not the common synth function, as it needs to return an environment with bindings.
%% Discard equality rules for now, so we cannot infer examples like the following one,
%% where X = {a, b} and Y should be inferred to be {a, b}.
%%
%%  {{a, b}=X, Y} when X == Y
%%
synth_env_guards(LocalEnv, Clause) ->
    Vars = mailbox_ast:clause_vars(Clause),
    ReturnEnv = mailbox_env:localenv_from_list(
                  [{mailbox_ast:var_name(V), mailbox_types:dyn_type()} || V <- Vars]),

    Guards = mailbox_ast:clause_guard(Clause),
    synth_env_guards(LocalEnv, Guards, ReturnEnv).


synth_env_guards(LocalEnv, Node, ReturnEnv) ->
    case mailbox_ast:type(Node) of
        'try' ->
            Arg = mailbox_ast:try_arg(Node),    %check if correct?
            synth_env_guards(LocalEnv, Arg, ReturnEnv);
        'let' ->
            RetEnv1 = synth_env_guards(LocalEnv, mailbox_ast:let_arg(Node), ReturnEnv),
            synth_env_guards(LocalEnv, mailbox_ast:let_body(Node), RetEnv1);
        'literal' ->
            ReturnEnv;
        'call' ->
            ModLit = mailbox_ast:call_module(Node),
            'erlang' = mailbox_ast:concrete(ModLit),

            FnNameLit = mailbox_ast:call_name(Node),
            case mailbox_ast:is_literal(FnNameLit) of
                false ->
                    ReturnEnv;
                true ->
                    FnName = mailbox_ast:concrete(FnNameLit),
                    case synth_bif(FnName) of
                        unknown ->
                            ReturnEnv;
                        {ok, Typ} ->
                            [Var] = mailbox_ast:call_args(Node),
                            VarName = mailbox_ast:var_name(Var),
                            mailbox_env:put_var(VarName, Typ, ReturnEnv)
                    end
            end
    end.


synth_bif(is_binary)        -> {ok, mailbox_types:bin_type()};
synth_bif(is_bitstring)     -> {ok, mailbox_types:bitstring_type()};
synth_bif(is_boolean)       -> {ok, mailbox_types:boolean_type()};
synth_bif(is_float)         -> {ok, mailbox_types:float_type()};
synth_bif(is_function)      -> {ok, mailbox_types:function_type()};
synth_bif(is_integer)       -> {ok, mailbox_types:integer_type()};
synth_bif(is_list)          -> {ok, mailbox_types:list_type()};
synth_bif(is_map)           -> {ok, mailbox_types:map_type()};
synth_bif(is_map_key)       -> {ok, mailbox_types:map_type()};
synth_bif(is_number)        -> {ok, mailbox_types:number_type()};
synth_bif(is_pid)           -> {ok, mailbox_types:pid_type()};
synth_bif(is_port)          -> {ok, mailbox_types:port_type()};
synth_bif(is_process_alive) -> {ok, mailbox_types:pid_type()};
synth_bif(is_record)        -> {ok, mailbox_types:record_type()};
synth_bif(is_reference)     -> {ok, mailbox_types:reference_type()};
synth_bif(is_tuple)         -> {ok, mailbox_types:tuple_type()};
synth_bif(_)                -> unknown.


synth_bindings(Env, LocalEnv, Node) ->
    case mailbox_ast:free_vars(Node) of
        [_|_] ->
            synth_env(Env, LocalEnv, Node);
        [] ->
            mailbox_env:new_localenv()
    end.


%%% ===================================================================
%%% Error Reporting
%%% ===================================================================

-doc """
Raise a typing error. Unwinds to the nearest `check_module` boundary,
which converts it into `{error, Reason}` for the failing function.
""".
-spec fail(Reason :: term()) -> no_return().
fail(Reason) ->
    throw(?ERR_TYPE_ERROR(Reason)).

%%% ===================================================================
%%% Error Formatting
%%% ===================================================================

-doc """
Render a checker error into a human-readable iolist message.

Use with `io:format("~s~n", [mailbox_check:format_error(Reason)])`.
""".
-spec format_error(Reason :: term()) -> iolist().
format_error({not_a_subtype, GotTy, ExpectedTy, Expr}) ->
    io_lib:format(
      "Type error: expected '~s', but got '~s'~n  in expression: '~s'",
      [mailbox_types:format_type(ExpectedTy),
       mailbox_types:format_type(GotTy),
       mailbox_ast:format(Expr)]);
format_error(Other) ->
    io_lib:format("Type error: ~p", [Other]).
