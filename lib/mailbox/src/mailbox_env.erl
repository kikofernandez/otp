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
%% Helper module to create and interact with the typing environment.
%%
%% The module covers helper functions to interact with a typing environment.
%% Some other functions simply help to create a module-level environment
%% from which to read type specs, mailbox specs, or user-defined types.
%%
-module(mailbox_env).
-moduledoc false.

-export([from_core/2, fetch_defs/1, build_fun_env/2]).
-export([lookup_spec/2, lookup_mailbox/2, lookup_type/2, lookup_modules/2]).
-export([lookup_return/2, lookup_args/2, fun_type/2]).
-export([merge_env/2, merge_env_meet/2, new_localenv/0, localenv_from_list/1, fold/3]).
-export([get_var/2, put_var/3]).

-include("c_types.hrl").

-record(env, {specs    = #{} :: #{{atom(), arity()} => c_types()},
              mailbox  = #{} :: #{atom() => c_types()},
              modules  = #{} :: #{module() => #{atom() => c_types()}},
              types    = []  :: [{atom() | tuple(), tuple()}]}).

-type env() :: #env{}.
-type var_name() :: atom() | integer() | {atom(), arity()}.
-type local_env() :: #{var_name() => c_types()}.
-export_type([env/0, local_env/0, var_name/0]).

-define(SPEC, spec).
-define(MB, mailbox).
-define(TYPE, type).

-doc """
Collects module definitions of specs, mailboxes, and types.

If `Opts` contains `all_available`, the specs of all loadable modules
are imported (slow — compiles every available module). Otherwise only
the module's own specs/mailboxes/types are collected and the remote
module database is left empty.
""".
-spec from_core(mailbox_ast:ast(), [Opt]) -> env() when Opt :: all_available | term().
from_core(Core, Opts) ->
    Attrs = mailbox_ast:module_attrs(Core),
    #env{specs = collect_specs(Attrs),
         mailbox = collect_mailboxes(Attrs),
         modules = load_all_available(Opts),
         types = collect_types(Attrs)}.

load_all_available(Opts) when is_list(Opts) ->
    case lists:member(all_available, Opts) of
        true  -> load_all_available();
        false -> #{}
    end.

load_all_available() ->
    Preloaded = erlang:pre_loaded(),
    Code = code:all_available(),
    Processes = 6,
    BucketSize = length(Code) div Processes,
    {L1, L2_} = lists:split(BucketSize, Code),
    {L2, L3_} = lists:split(BucketSize, L2_),
    {L3, L4_} = lists:split(BucketSize, L3_),
    {L4, L5_} = lists:split(BucketSize, L4_),
    {L5, L6} = lists:split(BucketSize, L5_),

    F = fun (I,Acc) -> collect_module_specs(Preloaded, I, Acc) end,
    Parent = self(),
    spawn(fun () -> Parent ! lists:foldl(F, #{}, L1) end),
    spawn(fun () -> Parent ! lists:foldl(F, #{}, L2) end),
    spawn(fun () -> Parent ! lists:foldl(F, #{}, L3) end),
    spawn(fun () -> Parent ! lists:foldl(F, #{}, L4) end),
    spawn(fun () -> Parent ! lists:foldl(F, #{}, L5) end),
    spawn(fun () -> Parent ! lists:foldl(F, #{}, L6) end),
    collect_messages(Processes).

collect_messages(Counter) when is_number(Counter) ->
    collect_messages(Counter, #{}).

collect_messages(0, Acc) ->
    Acc;
collect_messages(Counter, Acc) when Counter > 0 ->
    receive
        M1 ->
            collect_messages(Counter-1, maps:merge(Acc, M1))
    end.


collect_module_specs(Preloaded, {Name, Beam, _}, Acc) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {Mod, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            {ok, _, Core} = compile:forms(Forms, [to_core, binary]),
            Attrs = mailbox_ast:module_attrs(Core),
            Acc#{Mod => collect_specs(Attrs)};
        _Error ->
            Name0 = list_to_atom(Name),
            case module_source(Name0, Preloaded) of
                {ok, File} ->
                    Acc#{Name0 => specs_from_source(File)};
                error ->
                    %% ignore
                    Acc
            end
    end.


otp_root() ->
    case application:get_env(mailbox, otp_root) of
        {ok, Dir} -> Dir;
        undefined -> code:root_dir()
    end.


module_source(Name, Preloaded) ->
    case lists:member(Name, Preloaded) of
        true ->
            File = filename:join([otp_root(), "erts", "preloaded", "src",
                                  atom_to_list(Name) ++ ".erl"]),
            case filelib:is_regular(File) of
                true -> {ok, File};
                false -> error
            end;
        false ->
            error
    end.

specs_from_source(File) ->
    io:format("File: ~p~n", [File]),
    {ok, Forms} = epp:parse_file(File, []),
    lists:foldl(
      fun ({attribute, _, spec, {FA, Cls}}, Acc0) ->
              Types = [mailbox_parser:from_abs_type(C) || C <- Cls],
              Acc0#{FA => mailbox_types:combine_fun_types(Types)};
          (_, Acc0) ->
              Acc0
        end, #{}, Forms).




-doc """
Look up the spec for a function.
""".
-spec lookup_spec(FA, Env) -> {ok, c_types()} | error when
      FA :: {Fname :: atom(), Arity :: arity()},
      Env :: env().
lookup_spec(FA, #env{specs = Specs}) ->
    maps:find(FA, Specs).


-doc """
Look up the mailbox type by name.
""".
-spec lookup_mailbox(Name :: atom(), Env :: env()) -> {ok, c_types()} | error.
lookup_mailbox(Name, #env{mailbox = MB}) when is_atom(Name) ->
    maps:find(Name, MB).

-spec lookup_modules(MFA, Env) -> {ok, c_types()} | error when
      MFA :: {Mod :: module(), Fname :: atom(), Arity :: arity()},
      Env :: env().
lookup_modules({M, F, A}, #env{modules = Modules}) ->
    case maps:find(M, Modules) of
        {ok, Specs} -> maps:find({F, A}, Specs);
        error       -> error
    end.

-doc """
Look up the type for a user-defined type.
""".
-spec lookup_type(Name :: atom(), Env :: env()) -> {ok, {tuple(), list()}} | error.
lookup_type(Name, #env{types = Types}) ->
    case lists:keyfind(Name, 1, Types) of
        {Name, TypeBody, Params} ->
            {ok, {TypeBody, Params}};
        false ->
            error
    end.


-doc """
Fetches module-level definitions, skipping the `module_info`.
""".
-spec fetch_defs(Core) -> Return when
      Core :: mailbox_ast:ast(),
      Return :: [{mailbox_ast:ast(), mailbox_ast:ast()}].
fetch_defs(Core) ->
    [{Name, Def} || {Name, Def} <- mailbox_ast:module_defs(Core),
            true == mailbox_ast:is_fun(Def),
            not is_module_info(Def)].


-doc """
Builds a local environment for a function body.
That is, maps each argument name to its spec argument type.
""".
-spec build_fun_env(Def :: mailbox_ast:ast(), Env :: env()) -> local_env().
build_fun_env(Def, Env) ->
    Args = mailbox_ast:fun_vars(Def),
    FA = fetch_fun_name(Def),
    case lookup_spec(FA, Env) of
        {ok, #funTy{args = SpecArgs}} ->
            maps:from_list(
              [{mailbox_ast:var_name(A), T} ||
                  {A, T} <- lists:zip(Args, SpecArgs)]);
        {ok, #funAnyTy{}} ->
            maps:from_list(
              [{mailbox_ast:var_name(A), mailbox_types:dyn_type()} ||
                  A <- Args]);
        {ok, #funAbsTy{}} ->
            maps:from_list(
              [{mailbox_ast:var_name(A), mailbox_types:dyn_type()} ||
                  A <- Args]);
        error ->
            maps:from_list(
              [{mailbox_ast:var_name(A), mailbox_types:dyn_type()} ||
                  A <- Args])
    end.

-doc """
Merge two local environments. Bindings in `Env1` take precedence
over bindings in `Env0` (shadowing).
""".
-spec merge_env(Env0 :: local_env(), Env1 :: local_env()) -> local_env().
merge_env(Env0, Env1) ->
    maps:merge(Env0, Env1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Internal Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_specs(Attrs) ->
    lists:foldl(
      fun ({C1, C2}, Acc) ->
              case mailbox_ast:is_literal(C1) andalso ?SPEC == mailbox_ast:concrete(C1) of
                  true ->
                      [{NameArity, Types}] = mailbox_ast:concrete(C2),
                      true = is_list(Types),
                      Types1 = [mailbox_parser:from_abs_type(Ty) || Ty <- Types, is_tuple(Ty)],
                      UnionTypes = mailbox_types:combine_fun_types(Types1),
                      Acc#{NameArity => UnionTypes};
                  false ->
                      Acc
              end
      end, #{}, Attrs).


collect_mailboxes(Attrs) ->
    lists:foldl(fun ({C1, C2}, Acc) ->
                        case mailbox_ast:is_literal(C1) andalso
                            ?MB == mailbox_ast:concrete(C1) of
                            true ->
                                Concrete = mailbox_ast:concrete(C2),
                                true = is_list(Concrete),
                                T = [{MBName, mailbox_parser:from_abs_type(Type)} || {MBName, Type, _} <- Concrete],
                                maps:merge(Acc, maps:from_list(T));
                            false ->
                                Acc
                        end
                end, #{}, Attrs).


collect_types(Attrs) ->
    lists:foldl(
      fun({C1, C2}, Acc) ->
              case mailbox_ast:concrete(C1) == ?TYPE of
                  true ->
                      Types = mailbox_ast:concrete(C2),
                      true = is_list(Types),
                      Types ++ Acc;
                  false ->
                      Acc
              end
      end, [], Attrs).

%% Returns the Function name and arity
fetch_fun_name(Def) ->
    Ann = mailbox_ast:get_ann(Def),
    FunctionHead = lists:search(fun (Attr) when is_tuple(Attr) ->
                                        'function' == element(1, Attr)
                                end, Ann),
    case FunctionHead of
        {value, {'function', {Name, Arity}}} when is_atom(Name),
                                                  is_number(Arity) ->
            {Name, Arity};
         false ->
            {error, not_found}
    end.

%% Returns is a Def is a `module_info` function.
is_module_info(Def) ->
    Ann = mailbox_ast:get_ann(Def),
    FilterFn = fun (Attr) when is_tuple(Attr) ->
                       case element(2, Attr) of
                           {'module_info', _} -> true;
                           _ -> false
                       end
               end,
    case lists:search(FilterFn, Ann) of
        {value, _} ->
            true;
        false ->
            false
    end.

-spec lookup_return(FA, Env) -> {ok, c_types()} | error when
      FA :: {atom(), arity()},
      Env :: env().
lookup_return(FA, Env) ->
    case lookup_spec(FA, Env) of
        {ok, Ty} ->
            case mailbox_types:fun_return(Ty) of
                error -> error;
                Ret   -> {ok, Ret}
            end;
        error ->
            error
    end.

-spec lookup_args(FA, Env) -> {ok, [c_types()]} | error when
      FA :: {atom(), arity()},
      Env :: env().
lookup_args(FA, Env) ->
    case lookup_spec(FA, Env) of
        {ok, Ty} ->
            case mailbox_types:fun_args(Ty) of
                error -> error;
                Args  -> {ok, Args}
            end;
        error ->
            error
    end.

-spec fun_type(mailbox_ast:ast(), env()) -> {ok, c_types()} | error.
fun_type(Def, Env) ->
    FA = fetch_fun_name(Def),
    lookup_spec(FA, Env).

-doc """
Look up the type of a variable by name in the local environment.
""".
-spec get_var(Name :: var_name(), Env :: local_env()) -> {ok, c_types()} | error.
get_var(Name, Env) when is_atom(Name);
                        is_number(Name) ->
    maps:find(Name, Env).

-doc """
Set the type of a variable in the local environment.
""".
-spec put_var(Name :: var_name(), Typ :: c_types(), Env :: local_env()) -> local_env().
put_var(Name, Typ, Env) ->
    maps:put(Name, Typ, Env).

-spec new_localenv() -> local_env().
new_localenv() ->
    #{}.

-spec localenv_from_list([{var_name(), c_types()}]) -> local_env().
localenv_from_list(Pairs) ->
    maps:from_list(Pairs).

-spec merge_env_meet(local_env(), local_env()) -> local_env().
merge_env_meet(Env0, Env1) ->
    maps:fold(fun (Name, Ty1, Acc) ->
                      case maps:find(Name, Acc) of
                          {ok, Ty0} ->
                              %% Found. place their meet type
                              Acc#{Name := mailbox_types:meet(Ty0, Ty1)};
                          error ->
                              %% Not Found. Keep binding
                              Acc#{Name => Ty1}
                      end
            end, Env0, Env1).

-spec fold(Fun, local_env(), [mailbox_ast:ast()]) -> local_env() when
      Fun :: fun ((mailbox_ast:ast(), local_env()) -> local_env()).
fold(Fun, LocalEnv, Nodes) ->
    lists:foldl(Fun, LocalEnv, Nodes).
